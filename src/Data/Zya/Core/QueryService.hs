{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Zya.Core.QueryService(
  -- * Query service that process query requests
  queryService

  ) where


import Control.Applicative((<$>), liftA2, pure)
import Control.Concurrent.STM
import Control.Distributed.Process as Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Debug(traceOn, systemLoggerTracer, logfileTracer,traceLog)
import Control.Distributed.Process.Node as Node
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Binary
import Data.Data
import Data.Monoid((<>))
import Data.Text(pack, unpack, Text)
import Data.Time(UTCTime, getCurrentTime)
import Data.Typeable
import Data.Zya.Core.Service
import Data.Zya.Core.ServiceTypes
import Data.Zya.Persistence.Persistence(DBType, persist, ConnectionDetails(..))
import GHC.Generics (Generic)
import System.Environment(getArgs)
import Text.Printf
import Data.Zya.Utils.Logger


handleRemoteMessage :: Server -> DBType -> ConnectionDetails -> Maybe Int -> PMessage -> Process ()
handleRemoteMessage server dbType connectionString _ aMessage@(CreateTopic aTopic)  = do
  liftIO $ debugMessage $ pack ("Received message " <> show aMessage <> "\n")
  return ()


handleRemoteMessage server dbType connectionString _ aMessage@(ServiceAvailable serviceProfile pid) = do
  liftIO $ debugMessage $ pack  ("QueryService : Received Service Available message " <> show aMessage <> "\n")
  currentTime <- liftIO getCurrentTime
  _ <- liftIO $ atomically $ do
      myPid <- getMyPid server
      addService server serviceProfile pid
      sendRemote server pid (GreetingsFrom QueryService myPid, currentTime)
  return ()

handleRemoteMessage server dbType connectionString _ aMessage@(GreetingsFrom serviceProfile pid) = do
  liftIO $ do 
    debugMessage $ pack  ("Received message " <> show aMessage <> "\n")
    publishLocalSnapshot server pid

  return ()

handleRemoteMessage server dbType connectionString messageCount
  aMessage@(CommittedWriteMessage publisher (messageId, topic, message)) = do
    liftIO $ debugMessage $ pack  ("Handling remote message " <> show aMessage <> "\n")
    selfPid <- getSelfPid
    _ <- liftIO $ atomically $ updateMessageValue server messageId aMessage
    _ <- liftIO $ atomically $ updateMessageKey server selfPid messageId
    publishMessageKey <- liftIO $ atomically $ publishMessageKey server selfPid messageId
    messagesProcessed <- liftIO $ atomically $ queryMessageCount server
    liftIO $ debugMessage $ pack ("Total messages processed "
        <> show messagesProcessed <> " Max to be processed"
        <> show messageCount <> " "  <> "\n")


handleRemoteMessage server dbType connectionString messageCount
  aMessage@(WriteMessage publisher processId (messageId, topic, message, payload)) = do
  selfPid <- getSelfPid
  liftIO $ debugMessage $ pack  ("Received message " <> "Processor " <> show selfPid <> " " <> show aMessage <> "\n")
  return ()


handleRemoteMessage server dbType connectionString _ aMessage@(QueryMessage (messageId, processId, message)) = do
  liftIO $ debugMessage $ pack  ("Received message " <> show aMessage <> "\n")
  currentTime <- liftIO getCurrentTime
  _ <- liftIO $ atomically $ do
        messageValue <- queryMessageValue server messageId
        sendRemote server processId
          (QueryMessage (messageId, processId, messageValue), currentTime)
  return ()


handleRemoteMessage server dbType connectionString _ aMessage@(TerminateProcess message) = do
  liftIO $ debugMessage $ pack  ("Terminating self " <> show aMessage <> "\n")
  getSelfPid >>= flip exit (show aMessage)


handleRemoteMessage server dbType connectionString unhandledMessage _ =
  liftIO $ debugMessage $ pack  ("Received unhandled message  " <> show unhandledMessage <> "\n")


handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification server notificationMessage@(ProcessMonitorNotification _ pid _) = do
  liftIO $ debugMessage $ pack  ("Monitor notification " <> show notificationMessage <> "\n")
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
    forever
      (receiveWait
         [match $
            handleRemoteMessage serverL dbTypeL connectionDetailsL
              testMessageCount,
          match $ handleMonitorNotification serverL,
          matchIf (\ (WhereIsReply l _) -> l == sName) $
            handleWhereIsReply serverL QueryService,
          matchAny $ \ _ -> return ()])
      `Process.catchExit`
      (\ pId (TerminateProcess aText) ->
         do 
            liftIO $ errorMessage $ pack ("Terminating process " <> show aText <> " " <> show sName <> "\n")
            return ())

queryService :: ServerReaderT ()
queryService = do
  initializeProcess
  eventLoop
  return()
