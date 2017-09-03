{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.Subscription where

import GHC.Generics (Generic)
import System.Environment(getArgs)

import Control.Concurrent.STM
import Control.Applicative((<$>))
import Control.Exception

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Reader

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)

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
import Data.Zya.Core.Persistence.PersistZ

writerService :: ServerReaderT ()
writerService = undefined 

readerService :: ServerReaderT ()
readerService = undefined 

databaseService :: ServerReaderT ()
databaseService = undefined 

webservice :: ServerReaderT ()
webservice = undefined



handleRemoteMessage :: Server -> PMessage -> Process ()
handleRemoteMessage server aMessage = 
  say $ printf ("Received message " <> (show aMessage))

handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification server notificationMessage = 
  say $ printf ("Monitor notification " <> (show notificationMessage))


handleWhereIsReply _ (WhereIsReply _ Nothing) = return ()
handleWhereIsReply server (WhereIsReply _ (Just pid)) =
  liftIO $ atomically $ do
    --clientmap <- readTVar clients
    -- send our own server info,and request a response:
    return ()

{- | Terminate all processes calling exit on each -}
terminator :: ServerReaderT () 
terminator = do 
  (server, backend, profile, serviceName) <- ask
  remoteProcesses <- liftIO $ atomically $ remoteProcesses server
  lift $ do
    say $ printf "Terminator %s %s " (show profile) (show serviceName)
    forM_ remoteProcesses $ \peer -> exit peer $ TerminateProcess "Shutting down the cloud"
    pid <- getSelfPid -- the state is not update in the terminator, at least for now.
    exit pid $ TerminateProcess "Shutting down self"


subscription :: Backend -> (ServiceProfile, Text) -> Process ()
subscription backend (sP, params) = do
  n <- newServer
  let readerParams = (n, backend, sP, params) 
  say $ printf $ "Starting subscrpition " <> (show sP) <> (show params)
  case sP of
    Writer -> runReaderT writerService readerParams
    Reader -> runReaderT readerService readerParams
    DatabaseServer -> runReaderT databaseService readerParams
    WebServer ->  runReaderT webservice readerParams
    TopicAllocator -> runReaderT topicAllocator readerParams
    Terminator -> runReaderT terminator readerParams




remotable ['subscriptionService]


simpleBackend :: String -> String -> IO Backend 
simpleBackend = \a p -> initializeBackend a p $ Data.Zya.Core.Subscription.__remoteTable initRemoteTable

-- | For  example 'cloudEntryPoint (simpleBackend "localhost" "50000") (TopicAllocator, "ZYA")  '
cloudEntryPoint :: Backend -> (ServiceProfile, ServiceName) -> IO ()
cloudEntryPoint backend (sP, sName)= do
  node <- newLocalNode backend 
  Node.runProcess node (subscription backend (sP, sName))


parseArgs :: IO (ServiceProfile, Text, String)
parseArgs = do
  [serviceName, lparams, portNumber] <- getArgs
  let params = pack lparams
  return $ 
    case serviceName of 
      "Writer" -> (Writer, params, portNumber)
      "Reader" -> (Reader, params, portNumber)
      "Database" -> (DatabaseServer, params, portNumber) 
      "Webserver" -> (WebServer, params, portNumber)
      "TopicAllocator" -> (TopicAllocator, params, portNumber)
      "Terminator" -> (Terminator, params, portNumber)
      _  -> throw $ startupException $ pack $ "Invalid arguments " <> serviceName <> ":" <> lparams

cloudMain :: IO () 
cloudMain = do 
 (sProfile, sName, aPort) <- parseArgs
 backend <- simpleBackend "127.0.0.1" aPort
 cloudEntryPoint backend (sProfile, sName)
