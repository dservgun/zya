{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.TestWriter(
  -- * The cloud service used to test readers and writers.
  testWriter
  ) where



import Control.Applicative((<$>), liftA2)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node as Node
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State as State
import Control.Monad.Trans
import Control.Monad.Writer

import Data.Binary
import Data.Data
import Data.Map
import Data.Maybe
import Data.Monoid((<>))
import Data.Text(pack, unpack, Text)
import Data.Time(UTCTime, getCurrentTime)
import Data.Typeable
import Data.UUID.V1(nextUUID)
import Data.Zya.Core.LocalMessageHandlingStrategy(runMessageWriter)
import Data.Zya.Core.Service
import Data.Zya.Core.ServiceTypes
import Data.Zya.Utils.Logger as Logger

import GHC.Generics (Generic)
import System.Environment(getArgs)
import Text.Printf

newtype UUIDGenException =
    UUIDGenException{_unReason :: String} deriving Show
instance Exception UUIDGenException


componentName :: Text 
componentName = "Zya.Core.TestWriter"


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
  let messageCount = serverConfiguration^.numberOfTestMessages
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
  liftIO $ Logger.debugMessage  $ pack ("Received message " <> show aMessage <> "\n")
  currentTime <- liftIO getCurrentTime
  availableWriter <- liftIO $ atomically $ findAvailableWriter server
  case availableWriter of
    Just a -> liftIO $ atomically $ sendRemote server a (aMessage, currentTime)
    Nothing -> say $ printf $
                      "No writer found. Dropping this message " <> show aMessage <> "\n"

handleRemoteMessage server aCount aMessage@(ServiceAvailable serviceProfile pid) = do
  liftIO $ Logger.debugMessage  $ pack ("TestWriter : Received message " <> show aMessage <> "\n")
  say $ printf ("TestWriter : Received message " <> show aMessage <> "\n")
  currentTime <- liftIO getCurrentTime
  _ <- liftIO $ atomically $ do
      myPid <- getMyPid server
      addService server serviceProfile pid
      sendRemote server pid (GreetingsFrom TestWriter myPid, currentTime)
  return ()

handleRemoteMessage server aCount aMessage@(QueryMessage (messageId, processId, message)) =
  say $ printf ("Query Message handler : Received message " <> show message <> "\n")

handleRemoteMessage serverL aCount aMessage@(MessageKeyStore (messageId, processId)) = do
  myPid <- getSelfPid
  liftIO $ debugMessage  $ pack ("Test writer : Updating process key " <> show messageId <> "->" <> show processId <> "\n")
  currentTime <- liftIO getCurrentTime
  void $ liftIO $ atomically $ updateMessageKey serverL processId messageId
  _ <- liftIO $ atomically $
            sendRemote serverL processId (QueryMessage (messageId, myPid, Nothing), currentTime)
  say $ printf "Message key store message processed..\n"
  return()




handleRemoteMessage serverL aCount aMessage@(GreetingsFrom serviceProfileL pid) = do
  say $ printf ("Received message " <> show aMessage <> "\n")
  p <- liftIO $ atomically $ addService serverL serviceProfileL pid
  say $ printf "Added service " <> show p <> show serviceProfileL <> "\n"
  case serviceProfileL of
    Writer -> 
      case aCount of
        Just count -> 
          forM_ [1..count] $ \currentCount -> do
            say $ printf "Test Writer -> Writing message " <> show currentCount <> "\n"
            getNextMessage >>= flip runMessageWriter serverL
        Nothing -> return ()
    _ -> return ()

  return ()
  where
    getNextMessage = do
      myProcessId <- getSelfPid
      nextId <- liftIO nextUUID
      case nextId of
        Just nId -> do
          let topic = Topic $ pack "TestTopic"
          say $ printf "Writing message \n"
          return $
            WriteMessage
              (Publisher topic)
              myProcessId
              (pack $ show nId , topic, pack ("This is a test " <> show nId <> " : " <> show pid))
        Nothing -> throw $ UUIDGenException "No next id."

handleRemoteMessage _ _ unhandledMessage =
  say $ printf ("Received unhandled message  " <> show unhandledMessage <> "\n")


handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification serverL (ProcessMonitorNotification _ pid _) = do
  _ <- liftIO $ atomically $ removeProcess serverL pid
  terminate
