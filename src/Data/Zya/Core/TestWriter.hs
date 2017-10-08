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
import Control.Applicative((<$>), liftA2)
import Control.Exception

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State as State
import Control.Monad.Writer

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)

import Data.UUID.V1
import Data.Maybe
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
import Data.Zya.Core.LocalMessageHandlingStrategy(runMessageWriter)
import Data.Zya.Utils.Logger as Logger

newtype UUIDGenException = 
    UUIDGenException{_unReason :: String} deriving Show
instance Exception UUIDGenException


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
  liftIO $ Logger.setup "Zya.TestWriter"
  serverConfiguration <- ask
  let server1 = serverConfiguration^.server 
  let serviceNameStr = unpack $ serverConfiguration^.serviceName
  let profile = serverConfiguration^.serviceProfile
  let messageCount = serverConfiguration^.numberOfTestMessages
  lift $ say $ printf "Test writer event loop : " <> (show serviceNameStr) <> ": " <> 
        (show profile) <> ".\n" 
  lift $ do 
    let sName = serviceNameStr
    selfPid <- getSelfPid
    spawnLocal (proxyProcess server1)
    forever $
      receiveWait
        [ 
        match $ handleRemoteMessage server1 messageCount
        , match $ handleMonitorNotification server1
        , matchIf (\(WhereIsReply l _) -> l == sName) $ handleWhereIsReply server1 TestWriter
        , matchAny $ \_ -> return ()      -- discard unknown messages
        ]


handleRemoteMessage :: Server -> Maybe Int -> PMessage -> Process ()
handleRemoteMessage server aCount aMessage@(CreateTopic aTopic) = do
  liftIO $ Logger.debugMessage "Zya.core.TestWriter" $ printf ("Received message " <> (show aMessage) <> "\n")
  currentTime <- liftIO getCurrentTime
  availableWriter <- liftIO $ atomically $ findAvailableWriter server 
  case availableWriter of
    Just a -> liftIO $ atomically $ sendRemote server a (aMessage, currentTime)
    Nothing -> say $ printf $
                      "No writer found. Dropping this message " <> show aMessage <> "\n"


handleRemoteMessage server aCount aMessage@(ServiceAvailable serviceProfile pid) = do
  liftIO $ Logger.debugMessage "Zya.Core.TestWriter" ("TestWriter : Received message " <> (show aMessage) <> "\n")  
  say $ printf ("TestWriter : Received message " <> (show aMessage) <> "\n")  
  currentTime <- liftIO getCurrentTime
  _ <- liftIO $ atomically $ do 
      myPid <- getMyPid server
      addService server serviceProfile pid
      sendRemote server pid ((GreetingsFrom TestWriter myPid), currentTime)
  return ()

handleRemoteMessage server aCount aMessage@(QueryMessage (messageId, processId, message)) = do 
  say $ printf ("Query Message handler : Received message " <> show message <> "\n")

handleRemoteMessage server aCount aMessage@(MessageKeyStore (messageId, processId)) = do
  myPid <- getSelfPid

  liftIO $ debugMessage "Data.Zya.Core.TestWriter" ("Test writer : Updating process key " 
                <> (show messageId) <> "->" <> (show processId) <> "\n")
  currentTime <- liftIO getCurrentTime
  void $ liftIO $ atomically $ updateMessageKey server processId messageId
  _ <- liftIO $ atomically $ 
            sendRemote server processId ((QueryMessage (messageId, myPid, Nothing)), currentTime)  
  say $ printf ("Message key store message processed..\n")
  return()


handleRemoteMessage server aCount aMessage@(GreetingsFrom serviceProfile pid) = do
  say $ printf ("Received message " <> (show aMessage) <> "\n")
  p <- liftIO $ atomically $ addService server serviceProfile pid
  say $ printf "Added service " <> show p <> show serviceProfile <> "\n"
  case serviceProfile of 
    Writer -> do  
      case aCount of 
        Just count -> do 
          forM_ [1..count] $ \currentCount -> do 
            say $ printf "Test Writer -> Writing message " <> show currentCount <> "\n"
            getNextMessage >>= flip runMessageWriter server
        Nothing -> return ()
    _ -> return ()

  return ()
  where
    getNextMessage = do 
      nextId <- fmap id $ liftIO nextUUID 
      case nextId of 
        Just nId -> do 
          let topic = Topic $ pack "TestTopic"
          say $ printf "Writing message \n" 
          return $ 
            WriteMessage 
              (Publisher topic) 
              (pack $ show nId , topic, pack ("This is a test " <> show nId <> " : " <> show pid))
        Nothing -> throw $ UUIDGenException "No next id."

handleRemoteMessage server _ unhandledMessage = 
  say $ printf ("Received unhandled message  " <> (show unhandledMessage) <> "\n")


handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification server (ProcessMonitorNotification _ pid _) = do
  _ <- liftIO $ atomically $ removeProcess server pid 
  terminate
