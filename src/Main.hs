{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<**>))
import Control.Lens ((&), (.~), (<&>), (?~), (^.), view)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromMaybe)
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
    , rtdrsTaskDefinition
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
    , tdTaskDefinitionARN
    )
import Network.AWS.ECS.UpdateService
    ( updateService
    , usCluster
    , usTaskDefinition
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
import System.Environment (getProgName)
import System.Exit (die)
import Text.PrettyPrint.ANSI.Leijen
    ( Doc(..)
    , empty
    , fillBreak
    , indent
    , line
    , text
    )
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
    , _CmdOptService :: String
    , _CmdOptOldImageRegexp :: String
    , _CmdOptNewImage :: String
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
        (long "service" <> short 's' <> help "Name of service to update." <>
         metavar "SERVICE") <*>
    strOption
        (long "old-image" <> short 'o' <>
         help "Image to update the tag/digest for." <>
         metavar "IMAGE-REGEXP") <*>
    strOption
        (long "new-image" <> short 'n' <>
         help "New image to replace the old image with." <>
         metavar "IMAGE-PATH")

main :: IO ()
main = do
    progName <- getProgName
    config <- execParser (opts progName)
    updateImageForService config
  where
    opts progName =
        info
            (args <**> helper)
            (fullDesc <>
             progDesc
                 "Tool for updating image labels in ECS task \
                 \defintions in order to deploy new docker images." <>
             footerDoc (Just $ (footerDescription progName)))

footerDescription :: String -> Doc
footerDescription progName =
    (text "AWS credentials will be picked up from the environment or the AWS") <>
    (text "credentials file.") <>
    line <>
    line <>
    (text "Example usage: ") <>
    line <>
    line <>
    (indent
         4
         ((text progName) <>
          (text " --verbose -r Ireland -c mycluster -s myservice ") <>
          (text
               "-o myimage -n 054015229942.dkr.ecr.eu-west-1.amazonaws.com/myimage:label")))

updateImageForService (CmdArgs verbose region cluster service oldImageRegexp newImage) = do
    env <- newEnv Discover <&> envRegion .~ region
    serviceArn <- getMatchingServiceArn env (pack cluster) service verbose
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
    let taskData = extractParamsFromTaskDefinition definition
    when verbose $ do
        putStrLn "\nCurrent images in container definition:"
        print (map (\x -> x ^. cdImage) containerDefs)
    let updatedContainerDefs =
            (map (switchImage (pack oldImageRegexp) (pack newImage))
                 containerDefs)
    when verbose $ do
        putStrLn "\nImages after update:"
        print (map (\x -> x ^. cdImage) updatedContainerDefs)
    taskDefinitionArn <-
        registerTaskDefinitionAndReturnArn env taskData updatedContainerDefs
    updateServiceResponse <-
        runResourceT . runAWS env $
        send
            (updateService serviceArn & usCluster .~ (Just $ pack cluster) &
             usTaskDefinition .~ Just taskDefinitionArn)
    return ()

getMatchingServiceArn :: Env -> Text -> String -> Bool -> IO Text
getMatchingServiceArn env cluster service verbose = do
    servicesResponse <-
        runResourceT . runAWS env $ send (listServices & lsCluster ?~ cluster)
    let serviceArns = view lsrsServiceARNs servicesResponse
    when verbose $ do
        putStrLn "ECS services discovered:"
        mapM_ (putStrLn . unpack) serviceArns
    let matchingServiceArn =
            filter (\x -> unpack x =~ service :: Bool) serviceArns
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
            fromMaybe
                (error
                     "The task definition in AWS does not have the \
                        \family parameter set.")
                (taskDefinitionArn ^. tdFamily)
     in TaskData
            { _family = family
            , _compatibilities = taskDefinitionArn ^. tdRequiresCompatibilities
            , _networkMode = taskDefinitionArn ^. tdNetworkMode
            , _cpu = taskDefinitionArn ^. tdCpu
            , _memory = taskDefinitionArn ^. tdMemory
            , _executionRoleArn = taskDefinitionArn ^. tdExecutionRoleARN
            }

switchImage :: Text -> Text -> ContainerDefinition -> ContainerDefinition
switchImage oldImageRegexp newImage containerDefinition =
    let currentImage =
            fromMaybe
                (error "cdImage field not set in current task definition.")
                (containerDefinition ^. cdImage)
     in if unpack currentImage =~ ("(.*/.*:).*" :: String) :: Bool
            then containerDefinition & cdImage .~ Just newImage
            else containerDefinition

registerTaskDefinitionAndReturnArn ::
       Env -> TaskData -> [ContainerDefinition] -> IO Text
registerTaskDefinitionAndReturnArn env TaskData {..} updatedContainerDefs = do
    registerResponse <-
        runResourceT . runAWS env $
        send
            (registerTaskDefinition _family &
             rtdContainerDefinitions .~ updatedContainerDefs &
             rtdRequiresCompatibilities .~ _compatibilities &
             rtdNetworkMode .~ _networkMode &
             rtdCpu .~ _cpu &
             rtdMemory .~ _memory &
             rtdExecutionRoleARN .~ _executionRoleArn)
    when (registerResponse ^. rtdrsResponseStatus /= 200) $
        die "Failed to register task definition."
    let newTaskDefinition =
            fromMaybe (error "tjo") (registerResponse ^. rtdrsTaskDefinition)
        taskDefinitionArn =
            fromMaybe (error "tjo") (newTaskDefinition ^. tdTaskDefinitionARN)
    return taskDefinitionArn
