{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.Writer(
  -- * Writers that handle log events
  writer
  , createTopic
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

newtype CreateStatus = CreateStatus {_un :: Text}

type RemoteT = ReaderT (Server, PMessage) Process ()
{-| Internal type for persisting process messages-}
type MessageT = ReaderT (DBType, ConnectionDetails, PMessage) IO CreateStatus

{-| Transform the database error into a writer error. -}
mapError :: Either Text PMessage -> CreateStatus
mapError (Right e) = CreateStatus success
mapError (Left e) = CreateStatus e


createTopic :: MessageT
createTopic = do 
  -- Insert the topic into persistent store.
  -- return the status. Send the status to 
  -- some peers (need to decide that, could be all).
  p <- persist
  return $ mapError p


inform :: CreateStatus -> Process () 
inform = undefined


debugConnStr :: ConnectionDetails
debugConnStr = ConnectionDetails "host=localhost dbname=zya_debug user=zya_debug password=zya_debug port=5432"

handleRemoteMessage server aMessage@(CreateTopic aTopic) = do
  say $ printf ("Received message " <> (show aMessage))
  status <- liftIO $ runReaderT createTopic (defaultDb, debugConnStr, aMessage)
  -- Check the status and send a success or a failure to a group of 
  -- listeners: we need to set that up.
  inform status
  return ()


handleRemoteMessage server aMessage@(ServiceAvailable aTopic _) = do
  say $ printf ("Received message " <> (show aMessage))
  status <- liftIO $ runReaderT createTopic (defaultDb, debugConnStr, aMessage)
  -- Check the status and send a success or a failure to a group of 
  -- listeners: we need to set that up.
  inform status
  return ()

handleRemoteMessage server unhandledMessage = 
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
        match $ handleRemoteMessage serverL
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