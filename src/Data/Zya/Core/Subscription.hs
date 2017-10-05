{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.Subscription where

import GHC.Generics (Generic)
import System.Environment(getArgs)

import Control.Concurrent.STM
import Control.Applicative((<$>))
import Control.Exception

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Reader

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)

--If debug: how to set debug flags
import Control.Distributed.Process.Debug(traceOn, systemLoggerTracer, logfileTracer,traceLog, eventLogTracer)

import Data.Binary
import Data.Data
import Data.Monoid((<>))
import Data.Text(pack, unpack, Text)
import Data.Time(UTCTime, getCurrentTime)
import Data.Typeable
import Data.Zya.Core.Service

import Text.Printf
import Data.Zya.Core.ServiceTypes
import Data.Zya.Core.TopicAllocator
import Data.Zya.Core.Writer
import Data.Zya.Core.TestWriter
import Data.Zya.Core.QueryService

handleRemoteMessage :: Server -> PMessage -> Process ()
handleRemoteMessage server aMessage = do 
  selfPid <- getSelfPid
  say $ printf ("Received message " <> (show selfPid) <> " " <> (show aMessage) <> "\n")

handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification server notificationMessage = 
  say $ printf ("Monitor notification " <> (show notificationMessage) <> "\n")



readerService :: ServerReaderT () 
readerService = undefined


webService :: ServerReaderT () 
webService = undefined


tester :: ServerReaderT () 
tester = do 
  serverConfiguration <- ask 
  return () -- To be defined



{- | Terminate all processes calling exit on each -}
terminator :: ServerReaderT () 
terminator = do 
  serverConfiguration <- ask
  remoteProcesses <- liftIO $ atomically $ remoteProcesses (serverConfiguration^.server)

  lift $ do
    say $ printf "Terminator " <> (show $ serverConfiguration^.serviceProfile) 
          <> (show $ serverConfiguration^.serviceName)
          <> "\n"
    forM_ remoteProcesses $ \peer -> exit peer $ TerminateProcess "Shutting down the cloud"
    pid <- getSelfPid -- the state is not update in the terminator, at least for now.
    exit pid $ TerminateProcess "Shutting down self"


subscription :: Backend -> (ServiceProfile, Text, DBType, ConnectionDetails) -> Process ()
subscription backend (sP, params, dbType, dbConnection) = do
  --eventLogTracer  
  myPid <- getSelfPid
  n <- newServer myPid
  let readerParams = makeServerConfiguration n backend sP params dbType dbConnection
  say $ printf $ "Starting subscrpition " <> (show sP) <> (show params) <> "\n"
  case sP of
    Writer -> runReaderT writer readerParams
    Reader -> runReaderT readerService readerParams
    QueryService -> runReaderT queryService readerParams
    WebServer ->  runReaderT webService readerParams
    TopicAllocator -> runReaderT topicAllocator readerParams
    Terminator -> runReaderT terminator readerParams
    TestWriter -> runReaderT testWriter readerParams


remotable ['subscriptionService]

simpleBackend :: String -> String -> IO Backend 
simpleBackend = \a p -> initializeBackend a p $ Data.Zya.Core.Subscription.__remoteTable initRemoteTable

-- | For  example 'cloudEntryPoint (simpleBackend "localhost" "50000") (TopicAllocator, "ZYA")  '
cloudEntryPoint :: Backend -> (ServiceProfile, ServiceName, DBType, ConnectionDetails) -> IO ()
cloudEntryPoint backend (sP, sName, dbType, connectionDetails)= do
  node <- newLocalNode backend 
  Node.runProcess node (subscription backend (sP, sName, dbType, connectionDetails))


parseArgs :: IO (ServiceProfile, Text, String)
parseArgs = do
  [serviceName, lparams, portNumber] <- getArgs
  let params = pack lparams
  return $ 
    case serviceName of 
      "Writer" -> (Writer, params, portNumber)
      "Reader" -> (Reader, params, portNumber)
      "QueryService" -> (QueryService, params, portNumber) 
      "Webserver" -> (WebServer, params, portNumber)
      "TopicAllocator" -> (TopicAllocator, params, portNumber)
      "Terminator" -> (Terminator, params, portNumber)
      "TestWriter" -> (TestWriter, params, portNumber)
      _  -> throw $ startupException $ pack $ "Invalid arguments " <> serviceName <> ":" <> lparams

cloudMain :: IO () 
cloudMain = do 
 (sProfile, sName, aPort) <- parseArgs
 backend <- simpleBackend "127.0.0.1" aPort
 let dbType = RDBMS Postgresql
 let connectionDetails = ConnectionDetails "this connection wont work"
 cloudEntryPoint backend (sProfile, sName, dbType, connectionDetails)



