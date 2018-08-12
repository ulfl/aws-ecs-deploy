{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<**>))
import Control.Lens ((&), (.~), (<&>), (?~), (^.), view)
import qualified Data.ByteString.Char8 as BS8
import Data.Semigroup ((<>))
import Data.Text (Text, append, pack, unpack)
import Network.AWS
    ( AccessKey(..)
    , Credentials(FromKeys)
    , Region(Ireland)
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
import Network.AWS.Env (envRegion)
import Options.Applicative
import Safe (headMay)
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
    , _CmdOptDockerLabel :: String
    }

args :: Parser CmdArgs
args =
    CmdArgs <$>
    switch (long "verbose" <> short 'v' <> help "Print debug information.") <*>
    strOption
        (long "docker-label" <> short 'l' <>
         help "Replace the task image label with the provided label." <>
         metavar "LABEL")

main :: IO ()
main = deployImage =<< execParser opts
  where
    opts =
        info
            (args <**> helper)
            (fullDesc <> progDesc "Print a greeting for TARGET" <>
             header "hello - a test for optparse-applicative")

deployImage (CmdArgs _verbose imageLabel) = do
    let cluster = "minisites-cluster"
    e <-
        newEnv (FromFile "default" "/Users/ulf/.aws/credentials") <&> envRegion .~
        Ireland
    servicesResponse <-
        runResourceT . runAWS e $ send (listServices & lsCluster ?~ cluster)
    let serviceArns = view lsrsServiceARNs servicesResponse
        serviceArn =
            case headMay serviceArns of
                Just serviceArn' -> serviceArn'
                Nothing -> error "no service"
    print serviceArns
    print serviceArn
    describeServicesResponse <-
        runResourceT . runAWS e $
        send
            (describeServices & dCluster ?~ cluster & dServices .~ [serviceArn])
    let containerServices = view dssrsServices describeServicesResponse
        containerService =
            case headMay containerServices of
                Just containerService' -> containerService'
                Nothing -> error "no container service"
    -- print $ show containerService
    let taskDefinition =
            case view csTaskDefinition containerService of
                Just taskDefinition' -> taskDefinition'
                Nothing -> error "no task definition ARN"
    describeTaskDefinitionResponse <-
        runResourceT . runAWS e $ send (describeTaskDefinition taskDefinition)
    -- print $ view desrsTaskDefinition describeTaskDefinitionResponse
    putStrLn "task definition arn:"
    print taskDefinition
    putStrLn "task definition resp:"
    print describeTaskDefinitionResponse
    let definition =
            case describeTaskDefinitionResponse ^. desrsTaskDefinition of
                Just def -> def
                Nothing -> error "no task definition data"
    putStrLn "task definition2:"
    print definition
    let containerDefs = definition ^. tdContainerDefinitions
    putStrLn "container definitions:"
    print containerDefs
    let TaskData {..} = extractParamsFromTaskDefinition definition
    registerResponse <-
        runResourceT . runAWS e $
        send
            (registerTaskDefinition _family &
             rtdContainerDefinitions .~
             (map (switchImage (pack imageLabel)) containerDefs) &
             rtdRequiresCompatibilities .~ _compatibilities &
             rtdNetworkMode .~ _networkMode &
             rtdCpu .~ _cpu &
             rtdMemory .~ _memory &
             rtdExecutionRoleARN .~ _executionRoleArn)
    print registerResponse
    return ()

extractParamsFromTaskDefinition :: TaskDefinition -> TaskData
extractParamsFromTaskDefinition taskDefinition =
    let family =
            case taskDefinition ^. tdFamily of
                Just f -> f
                Nothing ->
                    error
                        "The task definition in AWS does not have the \
                        \family parameter set."
     in TaskData
            { _family = family
            , _compatibilities = taskDefinition ^. tdRequiresCompatibilities
            , _networkMode = taskDefinition ^. tdNetworkMode
            , _cpu = taskDefinition ^. tdCpu
            , _memory = taskDefinition ^. tdMemory
            , _executionRoleArn = taskDefinition ^. tdExecutionRoleARN
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
