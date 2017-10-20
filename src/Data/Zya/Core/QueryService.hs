{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Zya.Core.QueryService(
  -- * Query service that process query requests
  queryService

  ) where

import GHC.Generics (Generic)
import System.Environment(getArgs)

import Control.Concurrent.STM
import Control.Applicative((<$>), liftA2, pure)
import Control.Exception

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Reader

import Control.Distributed.Process as Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)

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



newtype RemoteMessageHandler a = RemoteMessageHandler {
  runApp :: ReaderT (Server, DBType, ConnectionDetails, PMessage) Process a
} deriving (
    Functor,
    Applicative,
    Monad,
    MonadIO
  )

handleRemoteMessage :: Server -> DBType -> ConnectionDetails -> Maybe Int -> PMessage -> Process ()
handleRemoteMessage server dbType connectionString _ aMessage@(CreateTopic aTopic)  = do
  say $  printf ("Received message " <> (show aMessage) <> "\n")
  return ()


handleRemoteMessage server dbType connectionString _ aMessage@(ServiceAvailable serviceProfile pid) = do
  say $  printf ("QueryService : Received Service Available message " <> (show aMessage) <> "\n")
  currentTime <- liftIO $ getCurrentTime
  _ <- liftIO $ atomically $ do
      myPid <- getMyPid server
      addService server serviceProfile pid
      sendRemote server pid $ (GreetingsFrom QueryService myPid, currentTime)
  return ()

handleRemoteMessage server dbType connectionString _ aMessage@(GreetingsFrom serviceProfile pid) = do
  say $  printf ("Received message " <> (show aMessage) <> "\n")
  liftIO $ publishLocalSnapshot server pid

  return ()

handleRemoteMessage server dbType connectionString messageCount
  aMessage@(CommittedWriteMessage publisher (messageId, topic, message)) = do
    say $ printf ("Handling remote message " <> show aMessage <> "\n")
    selfPid <- getSelfPid
    _ <- liftIO $ atomically $ updateMessageValue server messageId aMessage
    _ <- liftIO $ atomically $ updateMessageKey server selfPid messageId
    publishMessageKey <- liftIO $ atomically $ publishMessageKey server selfPid messageId

    messagesProcessed <- liftIO $ atomically $ queryMessageCount server
    say $ printf("Total messages processed "
        <> (show messagesProcessed) <> " Max to be processed"
        <> (show messageCount) <> " "  <> "\n")


handleRemoteMessage server dbType connectionString messageCount
  aMessage@(WriteMessage publisher (messageId, topic, message)) = do
  selfPid <- getSelfPid
  say $  printf ("Received message " <> "Processor " <> (show selfPid) <> " " <> (show aMessage) <> "\n")
  return ()


handleRemoteMessage server dbType connectionString _ aMessage@(QueryMessage (messageId, processId, message)) = do
  say $ printf ("Received message " <> show aMessage <> "\n")
  currentTime <- liftIO getCurrentTime
  _ <- liftIO $ atomically $ do
        messageValue <- queryMessageValue server messageId
        sendRemote server processId
          ((QueryMessage (messageId, processId, messageValue)), currentTime)
  return ()


handleRemoteMessage server dbType connectionString _ aMessage@(TerminateProcess message) = do
  say $ printf ("Terminating self " <> show aMessage <> "\n")
  getSelfPid >>= flip exit (show aMessage)


handleRemoteMessage server dbType connectionString unhandledMessage _ =
  say $  printf ("Received unhandled message  " <> (show unhandledMessage) <> "\n")


handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification server notificationMessage@(ProcessMonitorNotification _ pid _) = do
  say $  printf ("Monitor notification " <> (show notificationMessage) <> "\n")
  void $ liftIO $ atomically $ removeProcess server pid
  terminate

eventLoop :: ServerReaderT ()
eventLoop = do
  serverConfiguration <- ask
  lift $ do
    let sName = unpack $ serverConfiguration^.serviceName
    let serverL = serverConfiguration^.server
    let profileL = serverConfiguration^.serviceProfile
    let dbTypeL = serverConfiguration^.dbType
    let connectionDetailsL = serverConfiguration^.connDetails
    let testMessageCount = serverConfiguration^.numberOfTestMessages
    spawnLocal (proxyProcess serverL)
    (forever $
      receiveWait
        [
        match $ handleRemoteMessage serverL dbTypeL connectionDetailsL testMessageCount
        , match $ handleMonitorNotification serverL
        , matchIf (\(WhereIsReply l _) -> l == sName) $
                handleWhereIsReply serverL QueryService
        , matchAny $ \_ -> return ()      -- discard unknown messages
        ]) `Process.catchExit`
              (\pId (TerminateProcess aText) -> do
                  say $ printf ("Terminating process " <> show aText <> " " <> show sName <> "\n")
                  return ())

queryService :: ServerReaderT ()
queryService = do
  initializeProcess
  eventLoop
  return()
