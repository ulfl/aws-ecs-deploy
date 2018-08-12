{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text, pack)
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
    , dtdTaskDefinition
    , desrsTaskDefinition
    )
import Network.AWS.ECS.ListServices (listServices, lsCluster, lsrsServiceARNs)
import Network.AWS.ECS.RegisterTaskDefinition (registerTaskDefinition)
import Network.AWS.ECS.Types (csTaskDefinition)
import Network.AWS.Env (envRegion)
import Safe (headMay)

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
                Nothing -> error "no task definition"
    -- describeTaskDefinitionResponse <-
    --     runResourceT . runAWS e $
    --     send (describeTaskDefinition & dtdTaskDefinition .~ taskDefinition)
    -- print $ view desrsTaskDefinition describeTaskDefinitionResponse
    return ()
