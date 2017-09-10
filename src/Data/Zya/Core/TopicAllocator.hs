{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.TopicAllocator(
  -- * The cloud service allocating topics to writers and readers.
  topicAllocator
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




handleRemoteMessage :: Server -> PMessage -> Process ()
handleRemoteMessage server aMessage@(CreateTopic aTopic) = do
  say $ printf ("Received message " <> (show aMessage))
  -- Check for writers and then send a message to the writer.
  availableWriter <- liftIO $ atomically $ findAvailableWriter server 
  case availableWriter of
    Just a -> liftIO $ atomically $ sendRemote server a aMessage
    Nothing -> say $ printf $
                      "No writer found. Dropping this message " <> (show aMessage)
handleRemoteMessage server unhandledMessage = 
  say $ printf ("Received unhandled message  " <> (show unhandledMessage))

handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification server (ProcessMonitorNotification _ pid _) = do
  _ <- liftIO $ atomically $ removeProcess server pid 
  return ()


handleWhereIsReply _ (WhereIsReply _ Nothing) = return ()
handleWhereIsReply server (WhereIsReply _ (Just pid)) = do
  mSpid <- 
    liftIO $ atomically $ do
    mySpId <- readTVar $ myProcessId server
    sendRemote server pid (ServiceAvailable TopicAllocator mySpId)
    return mySpId
  say $ printf "Sending info about self %s -> %s" (show mSpid) (show pid)

topicAllocationEventLoop :: ServerReaderT ()
topicAllocationEventLoop = do
  serverConfiguration <- ask
  let server1 = view server serverConfiguration
  let serviceName1 = view serviceName serverConfiguration
  let serviceNameS = unpack serviceName1
  let backendl = view backend serverConfiguration
  let profile = view serviceProfile serverConfiguration
  lift $ do 
    let sName = serviceNameS
    selfPid <- getSelfPid
    spawnLocal (proxyProcess server1)
    say $ 
      printf "Updating topic allocator %s, profile : %s" (show TopicAllocator) 
        (show (profile :: ServiceProfile)) 
    liftIO $ atomically $ do 
      updateTopicAllocator server1 selfPid TopicAllocator
    forever $
      receiveWait
        [ 
        match $ handleRemoteMessage server1
        , match $ handleMonitorNotification server1
        , matchIf (\(WhereIsReply l _) -> l == sName) $
                handleWhereIsReply server1
        , matchAny $ \_ -> return ()      -- discard unknown messages
        ]

{- | 
  What does the allocator do: 
  It comes up and updates its local cache with its own
  process id as an allocator and starts a topic allocator event loop
  that listens to the any peers announcing themselves as allocators. If there is a 
  conflict, the allocator will switch itself as a backup and publish a message 
  announcing that. Will this work? 
  Note about failure and reliability: there is probably a need to implementing
  some form of consensus : raft seems the less daunting option. Implement the 
  c-interface to a reference implementation or complete the implementation using
  CH convenience functions. In our current implementation, we will skip this to 
  get a basic understanding of the overall interactions with the system. 

-}


topicAllocator :: ServerReaderT ()
topicAllocator = do 
  serverConfiguration <- ask
  initializeProcess 
  topicAllocationEventLoop
  return ()
