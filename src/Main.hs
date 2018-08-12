{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import qualified Data.ByteString.Char8 as BS8
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
    , rtdMemory
    , rtdNetworkMode
    , rtdRequiresCompatibilities
    )
import Network.AWS.ECS.Types
    ( ContainerDefinition(..)
    , cdImage
    , csTaskDefinition
    , tdContainerDefinitions
    , tdCpu
    , tdExecutionRoleARN
    , tdMemory
    , tdNetworkMode
    , tdRequiresCompatibilities
    )
import Network.AWS.Env (envRegion)
import Safe (headMay)
import Text.Regex.TDFA ((=~))

main = do
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
    registerResponse <-
        runResourceT . runAWS e $
        send
            (registerTaskDefinition "bitbuybit" &
             rtdContainerDefinitions .~ (map (switchImage "abba") containerDefs) &
             rtdRequiresCompatibilities .~
             (definition ^. tdRequiresCompatibilities) &
             rtdNetworkMode .~ (definition ^. tdNetworkMode) &
             rtdCpu .~ (definition ^. tdCpu) &
             rtdMemory .~ (definition ^. tdMemory) &
             rtdExecutionRoleARN .~ (definition ^. tdExecutionRoleARN))
    print registerResponse
    return ()

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
