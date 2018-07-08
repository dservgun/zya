{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.TestWriter(
  -- * The cloud service used to test readers and writers.
  testWriter
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Reader

import Data.Zya.Core.Internal.RemoteCommand as RemoteCommand
import Data.Monoid((<>))
import Data.Text(pack, unpack, Text)
import Data.Time(getCurrentTime)
import Data.UUID.V1(nextUUID)
import Data.UUID.Types(UUID)
import Data.Zya.Core.LocalMessageHandlingStrategy
  (runMessageWriter, runComputeNodeEvent)
import Data.Zya.Core.Service as ZService
import Data.Zya.Utils.Logger as Logger
import Data.Zya.Utils.ComponentDetails(ComponentName(..))


newtype UUIDGenException =
    UUIDGenException{_unReason :: String} deriving Show
instance Exception UUIDGenException


data TestWriterComponent = TestWriterComponent deriving (Show)

instance ComponentName TestWriterComponent where 
  componentName TestWriterComponent = "Data.Zya.Core.TestWriter"


{-| Test writer to send a few messages -}
-- Find an available writer, if none found, error out.
-- If one found, send one or more test messages.
testWriter :: ZService.ServerReaderT ()
testWriter = do
  let nSeconds = 10 :: Int
  initializeProcess
  eventLoop
  liftIO $ threadDelay $ nSeconds * (10 ^ (6 :: Int))  

eventLoop :: ZService.ServerReaderT ()
eventLoop = do
  serverConfiguration <- ask
  let server1 = serverConfiguration^.server
  let messageCount = serverConfiguration^.numberOfTestMessages
  lift $ do
    let sName = unpack $ serverConfiguration^.serviceName
    _ <- spawnLocal (proxyProcess server1)
    forever $
      receiveWait
        [
        match $ handleRemoteMessage server1 messageCount
        , match $ handleMonitorNotification server1
        , matchIf (\(WhereIsReply l _) -> l == sName) $ handleWhereIsReply server1 TestWriter
        , matchAny $ \_ -> return ()      -- discard unknown messages
        ]

handleRemoteMessage :: ZService.Server -> Maybe Int -> ZService.PMessage -> Process ()
handleRemoteMessage server' _ aMessage@(ZService.CreateTopic _) = do
  liftIO $ Logger.debugMessage  $ pack ("Received message " <> show aMessage <> "\n")
  currentTime <- liftIO getCurrentTime
  availableWriter <- liftIO $ atomically $ findAvailableWriter server'
  case availableWriter of
    Just a -> liftIO $ atomically $ sendRemote server' a (aMessage, currentTime)
    Nothing -> liftIO $ debugMessage $ pack  $
                      "No writer found. Dropping this message " <> show aMessage <> "\n"

handleRemoteMessage server' _ aMessage@(ZService.ServiceAvailable sProfile pid) = do
  liftIO $ Logger.debugMessage  $ pack ("TestWriter : Received message " <> show aMessage <> "\n")
  liftIO $ debugMessage $ pack  ("TestWriter : Received message " <> show aMessage <> "\n")
  currentTime <- liftIO getCurrentTime
  _ <- liftIO $ atomically $ do
      myPid <- getMyPid server'
      _ <- addService server' sProfile pid
      sendRemote server' pid (GreetingsFrom TestWriter myPid, currentTime)
  return ()

handleRemoteMessage _ _ aMessage@(ZService.QueryMessage _) =
  liftIO $ debugMessage $ 
    pack  ("Query Message handler : Received message " <> show aMessage <> "\n")

handleRemoteMessage serverL _ (ZService.MessageKeyStore (messageId, processId)) = do
  myPid <- getSelfPid
  liftIO $ debugMessage  $ pack ("Test writer : Updating process key " <> show messageId <> "->" <> show processId <> "\n")
  currentTime <- liftIO getCurrentTime
  void $ liftIO $ atomically $ updateMessageKey serverL processId messageId
  _ <- liftIO $ atomically $
            ZService.sendRemote serverL processId (ZService.QueryMessage (messageId, myPid, Nothing), currentTime)
  liftIO $ debugMessage $ pack  "Message key store message processed..\n"
  return()

handleRemoteMessage serverL aCount aMessage@(ZService.GreetingsFrom serviceProfileL pid) = do
  liftIO $ debugMessage $ pack  ("Received message " <> show aMessage <> "\n")
  p <- liftIO $ atomically $ ZService.addService serverL serviceProfileL pid
  liftIO $ debugMessage $ pack ("Added service " <> show p <> show serviceProfileL <> "\n")
  case serviceProfileL of
    ZService.ComputeNode -> 
      case aCount of
        Just count -> 
          forM_ [1..count] $ \currentCount -> do
            liftIO $ debugMessage $ pack ("Test Writer -> Writing message " <> show currentCount <> "\n")
            --getNextMessage >>= flip runMessageWriter serverL
            getNextRemoteCommand >>= flip runComputeNodeEvent serverL
        Nothing -> return ()
    _ -> return ()
  return ()
  where
    getIdPair :: Process (ProcessId, Text)
    getIdPair = do 
      pid <- getSelfPid 
      uuid <- liftIO nextUUID
      case uuid of 
        Just nid -> return (pid, pack . show $ nid) 
        Nothing -> throw $ UUIDGenException "No next id."

    getNextRemoteCommand = do 
      (selfPId, nextId) <- getIdPair 
      let remoteCommand = RemoteCommand.defaultCommand nextId
      return $ 
        ZService.ComputeNodeEvent (nextId, selfPId, remoteCommand)
    getNextMessage = do
      selfPid'<- getSelfPid
      nextId <- liftIO nextUUID
      case nextId of
        Just nId -> do
          let topic = ZService.Topic $ pack "TestTopic"
          return $
            ZService.WriteMessage
              (ZService.Publisher topic)
              selfPid'
              (pack $ show nId , topic, 
                pack ("This is a test " <> show nId <> " : " <> show pid)
                , Nothing)
        Nothing -> throw $ UUIDGenException "No next id."


handleRemoteMessage _ _ unhandledMessage =
  liftIO $ debugMessage $ pack  ("Received unhandled message  " <> show unhandledMessage <> "\n")

handleMonitorNotification :: ZService.Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification serverL (ProcessMonitorNotification _ pid _) = do
  _ <- liftIO $ atomically $ ZService.removeProcess serverL pid
  terminate
