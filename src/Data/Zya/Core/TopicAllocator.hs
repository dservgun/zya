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
import Data.Map
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


handleRemoteMessage server aMessage@(ServiceAvailable serviceProfile pid) = do
  say $ printf ("Received message " <> (show aMessage))  
  _ <- liftIO $ atomically $ do 
      myPid <- getMyPid server
      addService server serviceProfile pid
      sendRemote server pid (GreetingsFrom TopicAllocator myPid)
  return ()

handleRemoteMessage server aMessage@(GreetingsFrom serviceProfile pid) = do
  say $ printf ("Received message " <> (show aMessage))
  _ <- liftIO $ atomically $ do 
    addService server serviceProfile pid
  return ()

handleRemoteMessage server unhandledMessage = 
  say $ printf ("Received unhandled message  " <> (show unhandledMessage))

handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification server (ProcessMonitorNotification _ pid _) = do
  _ <- liftIO $ atomically $ removeProcess server pid 
  return ()


topicAllocationEventLoop :: ServerReaderT ()
topicAllocationEventLoop = do
  serverConfiguration <- ask
  let server1 = serverConfiguration^.server 
  let serviceNameStr = unpack $ serverConfiguration^.serviceName
  let profile = serverConfiguration^.serviceProfile
  lift $ do 
    let sName = serviceNameStr
    selfPid <- getSelfPid
    spawnLocal $ proxyProcess server1
    say $ 
      printf "Updating topic allocator %s, profile : %s" (show TopicAllocator) 
        (show (profile)) 
    forever $
      receiveWait
        [ 
        match $ handleRemoteMessage server1
        , match $ handleMonitorNotification server1
        , matchIf (\(WhereIsReply l _) -> l == sName) $
                handleWhereIsReply server1 TopicAllocator
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
