{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<**>))
import Control.Lens ((&), (.~), (<&>), (?~), (^.), view)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as BS8
import Data.Semigroup ((<>))
import Data.Text (Text, append, pack, unpack)
import Network.AWS
    ( AccessKey(..)
    , Credentials(FromKeys)
    , Region(..)
    , RqBody
    , SecretKey(..)
    , newEnv
    , runAWS
    , runResourceT
    , send
    , toBody
    )
import Network.AWS.Auth (Credentials(..))
import Network.AWS.ECS.DescribeServices
    ( dCluster
    , dServices
    , describeServices
    , dssrsServices
    )
import Network.AWS.ECS.DescribeTaskDefinition
    ( describeTaskDefinition
    , desrsTaskDefinition
    , dtdTaskDefinition
    )
import Network.AWS.ECS.ListServices (listServices, lsCluster, lsrsServiceARNs)
import Network.AWS.ECS.RegisterTaskDefinition
    ( registerTaskDefinition
    , rtdContainerDefinitions
    , rtdCpu
    , rtdExecutionRoleARN
    , rtdFamily
    , rtdMemory
    , rtdNetworkMode
    , rtdRequiresCompatibilities
    , rtdrsResponseStatus
    )
import Network.AWS.ECS.Types
    ( Compatibility(..)
    , ContainerDefinition(..)
    , NetworkMode(..)
    , TaskDefinition(..)
    , cdImage
    , csTaskDefinition
    , tdContainerDefinitions
    , tdCpu
    , tdExecutionRoleARN
    , tdFamily
    , tdMemory
    , tdNetworkMode
    , tdRequiresCompatibilities
    )
import Network.AWS.Env (Env(..), envRegion)
import Options.Applicative
    ( Parser(..)
    , auto
    , execParser
    , footerDoc
    , fullDesc
    , help
    , helper
    , info
    , infoFooter
    , long
    , metavar
    , option
    , progDesc
    , short
    , strOption
    , switch
    )
import Safe (headMay)
import System.Exit (die)
import Text.PrettyPrint.ANSI.Leijen (empty, fillBreak, text)
import Text.Regex.TDFA ((=~))

data TaskData = TaskData
    { _family :: Text
    , _compatibilities :: [Compatibility]
    , _networkMode :: Maybe NetworkMode
    , _cpu :: Maybe Text
    , _memory :: Maybe Text
    , _executionRoleArn :: Maybe Text
    }

data CmdArgs = CmdArgs
    { _CmdOptVerbose :: Bool
    , _CmdOptRegion :: Region
    , _CmdOptCluster :: String
    , _CmdOptServiceRegexp :: String
    , _CmdOptDockerLabel :: String
    }

args :: Parser CmdArgs
args =
    CmdArgs <$>
    switch (long "verbose" <> short 'v' <> help "Print debug information.") <*>
    option
        auto
        (long "region" <> short 'r' <>
         help "AWS region the ECS cluster resides in." <>
         metavar "Ireland|NorthCalifornia|etc.") <*>
    strOption
        (long "cluster" <> short 'c' <>
         help "ECS cluster the service belongs to." <>
         metavar "CLUSTER") <*>
    strOption
        (long "service" <> short 's' <> help "Regexp identifying the service." <>
         metavar "SERVICE-REGEXP") <*>
    strOption
        (long "docker-label" <> short 'l' <>
         help "Docker label to replace the current task image label with." <>
         metavar "LABEL")

main :: IO ()
main = updateImageLabel =<< execParser opts
  where
    opts =
        info
            (args <**> helper)
            (fullDesc <>
             progDesc
                 "Tool for updating image labels in ECS task \
                 \defintions in order to deploy new docker images." <>
             footerDoc
                 (Just $
                  text "AWS credentials will be picked up " <>
                  "from the environment or the AWS credentials file."))

updateImageLabel (CmdArgs verbose region cluster serviceRegexp imageLabel) = do
    env <- newEnv Discover <&> envRegion .~ region
    serviceArn <- getMatchingServiceArn env (pack cluster) serviceRegexp verbose
    when verbose $ do
        putStrLn "\nSelected ECS service:"
        putStrLn (unpack serviceArn)
    taskDefinitionArn <-
        getTaskDefinitionArnForService env (pack cluster) serviceArn
    when verbose $ do
        putStrLn "\nCurrently configured task definition for service:"
        putStrLn (unpack taskDefinitionArn)
    describeTaskDefinitionResponse <-
        runResourceT . runAWS env $
        send (describeTaskDefinition taskDefinitionArn)
    let definition =
            case describeTaskDefinitionResponse ^. desrsTaskDefinition of
                Just def -> def
                Nothing -> error "no task definition data"
    let containerDefs = definition ^. tdContainerDefinitions
    let TaskData {..} = extractParamsFromTaskDefinition definition
    registerResponse <-
        runResourceT . runAWS env $
        send
            (registerTaskDefinition _family &
             rtdContainerDefinitions .~
             (map (switchImage (pack imageLabel)) containerDefs) &
             rtdRequiresCompatibilities .~ _compatibilities &
             rtdNetworkMode .~ _networkMode &
             rtdCpu .~ _cpu &
             rtdMemory .~ _memory &
             rtdExecutionRoleARN .~ _executionRoleArn)
    when (registerResponse ^. rtdrsResponseStatus /= 200) $ do
        die "Failed to register task definition."
    return ()

getMatchingServiceArn :: Env -> Text -> String -> Bool -> IO Text
getMatchingServiceArn env cluster serviceRegexp verbose = do
    servicesResponse <-
        runResourceT . runAWS env $ send (listServices & lsCluster ?~ cluster)
    let serviceArns = view lsrsServiceARNs servicesResponse
    when verbose $ do
        putStrLn "ECS services discovered:"
        mapM_ (putStrLn . unpack) serviceArns
    let matchingServiceArn =
            filter
                (\service -> unpack service =~ serviceRegexp :: Bool)
                serviceArns
    when (length matchingServiceArn == 0) (error "No matching services.")
    when
        (length matchingServiceArn > 1)
        (error "More than one matching service.")
    return $ head matchingServiceArn

getTaskDefinitionArnForService :: Env -> Text -> Text -> IO Text
getTaskDefinitionArnForService env cluster serviceArn = do
    describeServicesResponse <-
        runResourceT . runAWS env $
        send
            (describeServices & dCluster ?~ cluster & dServices .~ [serviceArn])
    let containerServices = view dssrsServices describeServicesResponse
        containerService =
            case headMay containerServices of
                Just containerService' -> containerService'
                Nothing -> error "no container service"
    -- print $ show containerService
    let taskDefinitionArn =
            case view csTaskDefinition containerService of
                Just taskDefinition' -> taskDefinition'
                Nothing -> error "no task definition ARN"
    return taskDefinitionArn

extractParamsFromTaskDefinition :: TaskDefinition -> TaskData
extractParamsFromTaskDefinition taskDefinitionArn =
    let family =
            case taskDefinitionArn ^. tdFamily of
                Just f -> f
                Nothing ->
                    error
                        "The task definition in AWS does not have the \
                        \family parameter set."
     in TaskData
            { _family = family
            , _compatibilities = taskDefinitionArn ^. tdRequiresCompatibilities
            , _networkMode = taskDefinitionArn ^. tdNetworkMode
            , _cpu = taskDefinitionArn ^. tdCpu
            , _memory = taskDefinitionArn ^. tdMemory
            , _executionRoleArn = taskDefinitionArn ^. tdExecutionRoleARN
            }

-- Expected format for image URL:
-- "054015229942.dkr.ecr.eu-west-1.amazonaws.com/bitbuybit:bfcdf9c"
switchImage :: Text -> ContainerDefinition -> ContainerDefinition
switchImage newDockerLabel containerDefinition =
    let currentImage =
            case containerDefinition ^. cdImage of
                Just x -> x
                Nothing ->
                    error "cdImage field not set in current task definition." :: Text
        firstPart =
            (unpack currentImage) =~ ("(.*/bitbuybit:).*" :: String) :: [[String]]
     in containerDefinition &
        cdImage .~ (Just (pack (firstPart !! 0 !! 1) `append` newDockerLabel))
