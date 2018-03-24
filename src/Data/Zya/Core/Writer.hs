{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.Writer(
  -- * Writers that handle log events
  writer
  , handleRemoteMessage
  ) where





import Control.Applicative((<$>))
import Control.Concurrent.STM
import Control.Distributed.Process hiding(catch)
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
import Data.Zya.Utils.Logger
import Data.Zya.Persistence.Persistence(DBType, ConnectionDetails, MessageT, persist)

import GHC.Generics (Generic)
import System.Environment(getArgs)
import Text.Printf


-- File path to actually do the logging.
-- Use conduits.
rootLocation :: FilePath
rootLocation = "./tmp"


persistMessage :: MessageT
persistMessage = action `catch` (\a@(SomeException e) -> exception a)
  where
  action = do
    persist
    return $ CreateStatus ("Success" :: Text)
  exception a  =
      return $ CreateStatus $ "failure" <> (pack . show $ a)


handleRemoteMessage :: Server -> DBType -> ConnectionDetails -> PMessage -> Process ()
handleRemoteMessage server dbType connectionString aMessage@(CreateTopic aTopic) = do
  say $  printf ("Received message " <> show aMessage <> "\n")
  return ()

handleRemoteMessage server dbType connectionString aMessage@(ServiceAvailable serviceProfile pid) = do
  say $  printf ("Received message " <> show aMessage <> "\n")
  currentTime <- liftIO getCurrentTime
  _ <- liftIO $ atomically $ do
      myPid <- getMyPid server
      addService server serviceProfile pid
      fireRemote server pid $
                GreetingsFrom Writer myPid
  return ()

handleRemoteMessage server dbType connectionString aMessage@(GreetingsFrom serviceProfile pid) = do
  say $  printf ("Writer : Received message " <> show aMessage <> "\n")
  liftIO $ atomically $ addService server serviceProfile pid
  --publish local state
  liftIO $ publishLocalSnapshot server pid
  return ()



-- When the write succeeds, find a query service to send the entire message to.
-- Update a map with writer information for a process.
handleRemoteMessage server dbType connectionString aMessage@(WriteMessage publisher processId (messageId, topic, message)) = do
  selfPid <- getSelfPid
  time <- liftIO getCurrentTime
  say $  printf ("Received message " <> "Processor " <> show selfPid <> " " <> show aMessage <> "\n")
  status <- liftIO $ runReaderT persistMessage (dbType, connectionString, aMessage)
  case status of
    CreateStatus "Success" -> do
        liftIO $ debugMessage $ pack  ("Persisted message with status " <> show status <> "\n")
        posProcessId <- liftIO $ atomically $ findAvailableService server QueryService RoundRobin
        liftIO $ debugMessage $ pack  ("Message persisted successfully " <> show status  <> " " <> "Using query service " <> show posProcessId <> "\n")

        case posProcessId of
          Just x -> liftIO $ atomically $ sendRemote server x (committedMessage, time)
          Nothing -> liftIO $ debugMessage $ pack  ("No process id found for QueryService " <> "\n")
        return ()
        where
          committedMessage = CommittedWriteMessage publisher (messageId, topic, message)
    CreateStatus "failure" -> do
        liftIO $ debugMessage $ pack  "Could not persist message\n"
        liftIO $ atomically $ sendRemote server processId (CommitFailedMessage publisher (messageId, topic, message), time)


handleRemoteMessage server dbType connectionString aMessage@(TerminateProcess message) = do
  liftIO $ debugMessage $ pack  ("Terminating self " <> show aMessage <> "\n")
  getSelfPid >>= flip exit (show aMessage)

handleRemoteMessage server dbType connectionString unhandledMessage =
  say $  printf ("Received unhandled message  " <> show unhandledMessage <> "\n")


handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification server notificationMessage@(ProcessMonitorNotification _ pid _) = do
  say $  printf ("Monitor notification " <> show notificationMessage <> "\n")
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
    spawnLocal (proxyProcess serverL)
    forever $
      receiveWait
        [
        match $ handleRemoteMessage serverL dbTypeL connectionDetailsL
        , match $ handleMonitorNotification serverL
        , matchIf (\(WhereIsReply l _) -> l == sName) $
                handleWhereIsReply serverL Writer
        , matchAny $ \_ -> return ()      -- discard unknown messages
        ] `catch` (\e@(SomeException e1) -> liftIO $ putStrLn $ "Exception " <> show e <> "\n")

writer :: ServerReaderT ()
writer = do
  initializeProcess
  eventLoop
  return()


-- Some synonyms
success :: Text
success = "Success"
