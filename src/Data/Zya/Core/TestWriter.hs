{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.TestWriter(
  -- * The cloud service used to test readers and writers.
  testWriter
  ) where

import GHC.Generics (Generic)
import System.Environment(getArgs)

import Control.Concurrent
import Control.Concurrent.Async
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

import Data.UUID.V1
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


--writeMessage :: Server -> PMessage -> Process ()
writeMessage writer server aMessage =  do
  say $ printf "Sending message " <> (show aMessage) <> "\n"
  currentTime <- liftIO getCurrentTime
  case writer of 
    Just x -> liftIO $ atomically $ sendRemote (server) x (aMessage, currentTime)
    Nothing -> say $ printf "No writer found. " <> "\n"

{-| Test writer to send a few messages -}
-- Find an available writer, if none found, error out.
-- If one found, send one or more test messages.
testWriter :: ServerReaderT () 
testWriter = do 
  initializeProcess
  eventLoop
  liftIO $ threadDelay (10 ^ 6 * 10) -- add a delay


eventLoop :: ServerReaderT ()
eventLoop = do
  serverConfiguration <- ask
  let server1 = serverConfiguration^.server 
  let serviceNameStr = unpack $ serverConfiguration^.serviceName
  let profile = serverConfiguration^.serviceProfile
  lift $ say $ printf "Test writer event loop : " <> (show serviceNameStr) <> ": " <> 
        (show profile) <> ".\n" 
  lift $ do 
    let sName = serviceNameStr
    selfPid <- getSelfPid
    spawnLocal (proxyProcess server1)
    forever $
      receiveWait
        [ 
        match $ handleRemoteMessage server1
        , match $ handleMonitorNotification server1
        , matchIf (\(WhereIsReply l _) -> l == sName) $ handleWhereIsReply server1 TestWriter
        , matchAny $ \_ -> return ()      -- discard unknown messages
        ]


handleRemoteMessage :: Server -> PMessage -> Process ()
handleRemoteMessage server aMessage@(CreateTopic aTopic) = do
  say $ printf ("Received message " <> (show aMessage) <> "\n")
  currentTime <- liftIO getCurrentTime
  availableWriter <- liftIO $ atomically $ findAvailableWriter server 
  case availableWriter of
    Just a -> liftIO $ atomically $ sendRemote server a (aMessage, currentTime)
    Nothing -> say $ printf $
                      "No writer found. Dropping this message " <> show aMessage <> "\n"


handleRemoteMessage server aMessage@(ServiceAvailable serviceProfile pid) = do
  say $ printf ("TestWriter : Received message " <> (show aMessage) <> "\n")  
  currentTime <- liftIO getCurrentTime
  _ <- liftIO $ atomically $ do 
      myPid <- getMyPid server
      addService server serviceProfile pid
      sendRemote server pid ((GreetingsFrom TestWriter myPid), currentTime)
  return ()
handleRemoteMessage server aMessage@(MessageKeyStore (messageId, processId)) = do
  say $ printf ("Test writer : Updating process key " 
                <> (show messageId) <> "->" <> (show processId) <> "\n")
  void $ liftIO $ atomically $ updateMessageKey server processId messageId
  return()




handleRemoteMessage server aMessage@(GreetingsFrom serviceProfile pid) = do
  say $ printf ("Received message " <> (show aMessage) <> "\n")
  p <- liftIO $ atomically $ addService server serviceProfile pid
  say $ printf "Added service " <> show p <> show serviceProfile <> "\n"
  case serviceProfile of 
    Writer -> replicateM_ 10 sendOneMessage 
    _ -> return ()
  return ()
  where
    sendOneMessage = do 
        nextId <- liftIO (nextUUID)
        writer <- liftIO $ atomically $ findAvailableWriter server 

        case nextId of 
          Just nId -> do 
            say $ printf "Next Id " <> (show nId) <> "\n"
            let topic = Topic $ pack "TestTopic"
            let nMessage =  
                  WriteMessage 
                    (Publisher topic) 
                    (pack $ show nId , topic, pack ("This is a test" <> show writer))
            say $ printf "Topic " <> show topic <>  show nMessage <> "\n"

            -- liftIO $ print "Message " <> show nMessage <> "\n"
            _ <- writeMessage writer server nMessage
            say $ printf "Before exiting..." <> "\n"
            return ()
          Nothing -> do 
            say $ printf "Nothing to print" <> "\n"

handleRemoteMessage server unhandledMessage = 
  say $ printf ("Received unhandled message  " <> (show unhandledMessage) <> "\n")

handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification server (ProcessMonitorNotification _ pid _) = do
  _ <- liftIO $ atomically $ removeProcess server pid 
  return ()
