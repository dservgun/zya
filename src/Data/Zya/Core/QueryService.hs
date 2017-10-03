{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.QueryService(
  -- * Query service that process query requests
  queryService
  , handleRemoteMessage
  ) where

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
import Control.Distributed.Process.Debug(traceOn, systemLoggerTracer, logfileTracer,traceLog)

import Data.Binary
import Data.Data
import Data.Monoid((<>))
import Data.Text(pack, unpack, Text)
import Data.Time(UTCTime, getCurrentTime)
import Data.Typeable
import Data.Zya.Core.Service
import Text.Printf
import Data.Zya.Core.ServiceTypes
import Data.Zya.Persistence.Persistence(DBType, persist)





persistMessage :: MessageT
persistMessage = do 
  -- Insert the topic into persistent store.
  -- return the status. Send the status to 
  -- some peers (need to decide that, could be all).
  p <- persist
  return $ CreateStatus "success??"



handleRemoteMessage :: Server -> DBType -> ConnectionDetails -> PMessage -> Process ()
handleRemoteMessage server dbType connectionString aMessage@(CreateTopic aTopic) = do
  traceLog $  printf ("Received message " <> (show aMessage) <> "\n")
  return ()

handleRemoteMessage server dbType connectionString aMessage@(ServiceAvailable serviceProfile pid) = do
  traceLog $  printf ("Received message " <> (show aMessage) <> "\n")  
  currentTime <- liftIO $ getCurrentTime
  _ <- liftIO $ atomically $ do 
      myPid <- getMyPid server
      addService server serviceProfile pid
      sendRemote server pid ((GreetingsFrom Writer myPid), currentTime)
  return ()

handleRemoteMessage server dbType connectionString aMessage@(GreetingsFrom serviceProfile pid) = do
  traceLog $  printf ("Received message " <> (show aMessage) <> "\n")
  _ <- liftIO $ atomically $ addService server serviceProfile pid
  return ()

handleRemoteMessage server dbType connectionString aMessage@(WriteMessage publisher (messageId, topic, message)) = do
  selfPid <- getSelfPid
  traceLog $  printf ("Received message " <> "Processor " <> (show selfPid) <> " " <> (show aMessage) <> "\n")
  -- Update local cache.
  return ()

handleRemoteMessage server dbType connectionString unhandledMessage = 
  traceLog $  printf ("Received unhandled message  " <> (show unhandledMessage) <> "\n")


handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification server notificationMessage@(ProcessMonitorNotification _ pid _) = do
  traceLog $  printf ("Monitor notification " <> (show notificationMessage) <> "\n")
  void $ liftIO $ atomically $ removeProcess server pid 


eventLoop :: ServerReaderT ()
eventLoop = do
  serverConfiguration <- ask
  lift $ do 
    let sName = unpack $ serverConfiguration^.serviceName
    let serverL = serverConfiguration^.server 
    let profileL = serverConfiguration^.serviceProfile
    let dbTypeL = serverConfiguration^.dbType 
    let connectionDetailsL = serverConfiguration^.connDetails
    spawnLocal (proxyProcess serverL)
    forever $
      receiveWait
        [ 
        match $ handleRemoteMessage serverL dbTypeL connectionDetailsL
        , match $ handleMonitorNotification serverL
        , matchIf (\(WhereIsReply l _) -> l == sName) $
                handleWhereIsReply serverL Writer
        , matchAny $ \_ -> return ()      -- discard unknown messages
        ]

queryService :: ServerReaderT () 
queryService = do
  initializeProcess
  eventLoop
  return()


-- Some synonyms
success :: Text 
success = "Success"
