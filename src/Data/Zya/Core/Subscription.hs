{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.Subscription where

import System.Environment(getArgs)

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Exception.Safe

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)


import Data.Monoid((<>))
import Data.Text(pack, Text)
import Data.Zya.Core.Service

import Text.Printf
import Data.Zya.Core.TopicAllocator
import Data.Zya.Core.Writer
import Data.Zya.Core.TestWriter
import Data.Zya.Core.QueryService
import Data.Zya.Core.WebServerService(webService)
import Data.Zya.Core.ComputeNodeService(computeService)

newtype UnsupportedServiceException = UnsupportedServiceException {_unServiceType :: Maybe ServiceProfile} deriving(Show)
instance Exception UnsupportedServiceException

handleRemoteMessage :: Server -> PMessage -> Process ()
handleRemoteMessage _ aMessage = do
  selfPid <- getSelfPid
  say $ printf ("Received message " <> show selfPid <> " " <> show aMessage <> "\n")

handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification _ notificationMessage =
  say $ printf ("Monitor notification " <> show notificationMessage <> "\n")





{- | Terminate all processes calling exit on each -}
terminator :: ServerReaderT ()
terminator = do
  serverConfiguration <- ask
  remoteProcessesL <- liftIO $ atomically $ remoteProcesses (serverConfiguration^.server)
  lift $ do 
    say $ printf "Terminator " <> show (serverConfiguration ^. serviceProfile)
      <> show (serverConfiguration^.serviceName)
      <> "\n"
    forM_ remoteProcessesL $ \peer -> exit peer $ TerminateProcess "Shutting down the cloud"
    pid <- getSelfPid -- the state is not update in the terminator, at least for now.
    exit pid $ TerminateProcess "Shutting down self"


subscription :: Backend -> (ServiceProfile, Text, DBType, ConnectionDetails, Maybe Int, Int) -> Process ()
subscription backendL (sP, params, dbTypeL, dbConnection, count, portNumber) = do
  --eventLogTracer
  myPid <- getSelfPid
  n <- newServer myPid
  let readerParams = makeServerConfiguration n backendL sP params dbTypeL dbConnection count portNumber

  say $ printf $ "Starting subscrpition " <> show sP <> " " <> show params <> "\n"
  case sP of
    Writer -> runReaderT writer readerParams
    Reader -> Control.Exception.Safe.throw $ UnsupportedServiceException $ Just Reader
    QueryService -> runReaderT queryService readerParams
    WebServer ->  runReaderT webService readerParams
    TopicAllocator -> runReaderT topicAllocator readerParams
    Terminator -> runReaderT terminator readerParams
    TestWriter -> runReaderT testWriter readerParams
    ComputeNode -> runReaderT computeService readerParams
    _ -> Control.Exception.Safe.throw $ UnsupportedServiceException $ Just sP

remotable ['subscriptionService]

simpleBackend :: String -> String -> IO Backend
simpleBackend a p = initializeBackend a p $ Data.Zya.Core.Subscription.__remoteTable initRemoteTable


-- | For  example 'cloudEntryPoint (simpleBackend "localhost" "50000") (TopicAllocator, "ZYA")  '
cloudEntryPoint :: Backend ->
  (ServiceProfile, ServiceName, DBType,
    ConnectionDetails, Maybe Int, Int) -> IO ()
cloudEntryPoint backendL (sP, sName, dbTypeL, connectionDetails, count, portNumber)= do
  node <- newLocalNode backendL
  Node.runProcess node
    (subscription backendL (sP, sName, dbTypeL, connectionDetails, count, portNumber))


parseArgs :: IO (ServiceProfile, Text, String)
parseArgs = do
  [lserviceName, lparams, portNumber] <- getArgs
  let params = pack lparams
  return $
    case lserviceName of
      "Writer" -> (Writer, params, portNumber)
      "Reader" -> (Reader, params, portNumber)
      "QueryService" -> (QueryService, params, portNumber)
      "WebServer" -> (WebServer, params, portNumber)
      "TopicAllocator" -> (TopicAllocator, params, portNumber)
      "Terminator" -> (Terminator, params, portNumber)
      "TestWriter" -> (TestWriter, params, portNumber)
      _  -> (Unknown, params, portNumber)

cloudMain :: IO ()
cloudMain = do
 (sProfile, sName, aPort) <- parseArgs
 backendL <- simpleBackend "127.0.0.1" aPort
 let dbTypeL = RDBMS Postgresql
 let connectionDetailsL = ConnectionDetails "this connection wont work"
 cloudEntryPoint backendL (sProfile, sName, dbTypeL, connectionDetailsL, Nothing, -1)



