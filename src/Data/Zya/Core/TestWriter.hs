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

import Data.Monoid((<>))
import Data.Text(pack, unpack, Text)
import Data.Time(getCurrentTime)
import Data.UUID.V1(nextUUID)
import Data.Zya.Core.LocalMessageHandlingStrategy(runMessageWriter)
import Data.Zya.Core.Service
import Data.Zya.Utils.Logger as Logger
import Data.Zya.Utils.ComponentDetails(ComponentName(..))


newtype UUIDGenException =
    UUIDGenException{_unReason :: String} deriving Show
instance Exception UUIDGenException


newtype TestWriterComponent = TestWriterComponent {_uncomp :: Text} 

instance ComponentName TestWriterComponent where 
  componentName (TestWriterComponent _compName) = _compName


{-| Test writer to send a few messages -}
-- Find an available writer, if none found, error out.
-- If one found, send one or more test messages.
testWriter :: ServerReaderT ()
testWriter = do
  let nSeconds = 10 :: Int
  initializeProcess
  eventLoop
  liftIO $ threadDelay $ nSeconds * (10 ^ (6 :: Int))  

eventLoop :: ServerReaderT ()
eventLoop = do
  serverConfiguration <- ask
  let server1 = serverConfiguration^.server
  let serviceNameStr = unpack $ serverConfiguration^.serviceName
  let messageCount = serverConfiguration^.numberOfTestMessages
  lift $ do
    let sName = serviceNameStr
    _ <- spawnLocal (proxyProcess server1)
    forever $
      receiveWait
        [
        match $ handleRemoteMessage server1 messageCount
        , match $ handleMonitorNotification server1
        , matchIf (\(WhereIsReply l _) -> l == sName) $ handleWhereIsReply server1 TestWriter
        , matchAny $ \_ -> return ()      -- discard unknown messages
        ]



handleRemoteMessage :: Server -> Maybe Int -> PMessage -> Process ()
handleRemoteMessage server' _ aMessage@(CreateTopic _) = do
  liftIO $ Logger.debugMessage  $ pack ("Received message " <> show aMessage <> "\n")
  currentTime <- liftIO getCurrentTime
  availableWriter <- liftIO $ atomically $ findAvailableWriter server'
  case availableWriter of
    Just a -> liftIO $ atomically $ sendRemote server' a (aMessage, currentTime)
    Nothing -> liftIO $ debugMessage $ pack  $
                      "No writer found. Dropping this message " <> show aMessage <> "\n"

handleRemoteMessage server' _ aMessage@(ServiceAvailable sProfile pid) = do
  liftIO $ Logger.debugMessage  $ pack ("TestWriter : Received message " <> show aMessage <> "\n")
  liftIO $ debugMessage $ pack  ("TestWriter : Received message " <> show aMessage <> "\n")
  currentTime <- liftIO getCurrentTime
  _ <- liftIO $ atomically $ do
      myPid <- getMyPid server'
      _ <- addService server' sProfile pid
      sendRemote server' pid (GreetingsFrom TestWriter myPid, currentTime)
  return ()

handleRemoteMessage _ _ aMessage@(QueryMessage _) =
  liftIO $ debugMessage $ 
    pack  ("Query Message handler : Received message " <> show aMessage <> "\n")

handleRemoteMessage serverL _ (MessageKeyStore (messageId, processId)) = do
  myPid <- getSelfPid
  liftIO $ debugMessage  $ pack ("Test writer : Updating process key " <> show messageId <> "->" <> show processId <> "\n")
  currentTime <- liftIO getCurrentTime
  void $ liftIO $ atomically $ updateMessageKey serverL processId messageId
  _ <- liftIO $ atomically $
            sendRemote serverL processId (QueryMessage (messageId, myPid, Nothing), currentTime)
  liftIO $ debugMessage $ pack  "Message key store message processed..\n"
  return()




handleRemoteMessage serverL aCount aMessage@(GreetingsFrom serviceProfileL pid) = do
  liftIO $ debugMessage $ pack  ("Received message " <> show aMessage <> "\n")
  p <- liftIO $ atomically $ addService serverL serviceProfileL pid
  liftIO $ debugMessage $ pack ("Added service " <> show p <> show serviceProfileL <> "\n")
  case serviceProfileL of
    Writer -> 
      case aCount of
        Just count -> 
          forM_ [1..count] $ \currentCount -> do
            liftIO $ debugMessage $ pack ("Test Writer -> Writing message " <> show currentCount <> "\n")
            getNextMessage >>= flip runMessageWriter serverL
        Nothing -> return ()
    _ -> return ()

  return ()
  where
    getNextMessage = do
      selfPid'<- getSelfPid
      nextId <- liftIO nextUUID
      case nextId of
        Just nId -> do
          let topic = Topic $ pack "TestTopic"
          liftIO $ debugMessage $ pack  "Writing message \n"
          return $
            WriteMessage
              (Publisher topic)
              selfPid'
              (pack $ show nId , topic, pack ("This is a test " <> show nId <> " : " <> show pid))
        Nothing -> throw $ UUIDGenException "No next id."

handleRemoteMessage _ _ unhandledMessage =
  liftIO $ debugMessage $ pack  ("Received unhandled message  " <> show unhandledMessage <> "\n")


handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification serverL (ProcessMonitorNotification _ pid _) = do
  _ <- liftIO $ atomically $ removeProcess serverL pid
  terminate
