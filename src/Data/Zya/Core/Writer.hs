{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.Writer(
  -- * Writers that handle log events
  writer
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

import Data.Binary
import Data.Data
import Data.Monoid((<>))
import Data.Text(pack, unpack, Text)
import Data.Time(UTCTime, getCurrentTime)
import Data.Typeable
import Data.Zya.Core.Service
import Text.Printf
import Data.Zya.Core.ServiceTypes
import Data.Zya.Persistence.PersistZ
import Data.Zya.Persistence.Persistence(DBType, persist)


-- File path to actually do the logging.
-- Use conduits.
rootLocation :: FilePath 
rootLocation = "./tmp" 


type RemoteT = ReaderT (Server, PMessage) Process ()

{-| Transform the database error into a writer error. -}
mapError :: Either Text PMessage -> CreateStatus
mapError (Right e) = CreateStatus success
mapError (Left e) = CreateStatus e


persistMessage :: MessageT
persistMessage = do 
  -- Insert the topic into persistent store.
  -- return the status. Send the status to 
  -- some peers (need to decide that, could be all).
  p <- persist
  return $ CreateStatus "success??"


inform :: CreateStatus -> Process () 
inform = undefined



handleRemoteMessage server dbType connectionString aMessage@(CreateTopic aTopic) = do
  say $ printf ("Received message " <> (show aMessage))
  -- Check the status and send a success or a failure to a group of 
  -- listeners: we need to set that up.
  --inform status
  return ()


handleRemoteMessage server dbType connectionString aMessage@(ServiceAvailable aTopic _) = do
  say $ printf ("Received message " <> (show aMessage))
  -- Check the status and send a success or a failure to a group of 
  -- listeners: we need to set that up.
  --inform status
  return ()
handleRemoteMessage server dbType connectionString aMessage@(WriteMessage publisher (messageId, message)) = do
  say $ printf ("Received message " <> (show aMessage))
  status <- liftIO $ runReaderT persistMessage (dbType, connectionString, aMessage)
  say $ printf "Message persisted successfully %s " (show status)
  return ()

handleRemoteMessage server dbType connectionString unhandledMessage = 
  say $ printf ("Received unhandled message  " <> (show unhandledMessage))

handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification server notificationMessage = 
  say $ printf ("Monitor notification " <> (show notificationMessage))


handleWhereIsReply _ (WhereIsReply _ Nothing) = return ()
handleWhereIsReply server (WhereIsReply _ (Just pid)) =
  liftIO $ atomically $ do
    return ()


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
    say $ 
      printf "Updating topic allocator %s, profile : %s" (show TopicAllocator) 
        (show profileL) 
    liftIO $ atomically $ do 
      selfPid <- readTVar $ myProcessId serverL
      updateTopicAllocator serverL selfPid TopicAllocator
    forever $
      receiveWait
        [ 
        match $ handleRemoteMessage serverL dbTypeL connectionDetailsL
        , match $ handleMonitorNotification serverL
        , matchIf (\(WhereIsReply l _) -> l == sName) $
                handleWhereIsReply serverL
        , matchAny $ \_ -> return ()      -- discard unknown messages
        ]

writer :: ServerReaderT () 
writer = do
  initializeProcess
  eventLoop
  return()


-- Some synonyms
success :: Text 
success = "Success"
