
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.ComputeNodeService(
  -- * Writers that handle log events
  computeService
  , handleRemoteMessage
  ) where

import Control.Applicative((<$>))
import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Debug(traceOn, systemLoggerTracer, logfileTracer,traceLog)
import Control.Distributed.Process.Node as Node
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans as Trans
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
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I
import Data.Zya.Utils.Logger


componentName :: Text 
componentName = "Data.Zya.Core.ComputeNodeService"
handleRemoteMessage :: Server -> DBType -> ConnectionDetails -> PMessage -> Process ()
handleRemoteMessage server dbType connectionString aMessage@(CreateTopic aTopic) = do
  liftIO $ debugMessage  $ pack ("Received message " <> show aMessage <> "\n")
  return ()

handleRemoteMessage server dbType connectionString aMessage@(ServiceAvailable serviceProfile pid) = do
  liftIO $ debugMessage  $ pack ("Received message " <> show aMessage <> "\n")
  currentTime <- liftIO getCurrentTime
  void . liftIO $ atomically $ do
      myPid <- getMyPid server
      addService server serviceProfile pid
      fireRemote server pid $
                GreetingsFrom ComputeNode myPid

handleRemoteMessage server dbType connectionString aMessage@(GreetingsFrom serviceProfile pid) = do
  liftIO $ debugMessage  $ pack ("ComputeNode : Received message " <> show aMessage <> "\n")
  liftIO $ atomically $ addService server serviceProfile pid
  --publish local state
  liftIO $ publishLocalSnapshot server pid

  return ()



handleRemoteMessage server dbType connectionString 
  aMessage@(WriteMessage publisher processId (messageId, topic, message, paylaod)) = do
  selfPid <- getSelfPid
  time <- liftIO getCurrentTime
  liftIO $ debugMessage  $ pack ("Received message " <> "Processor " <> show selfPid <> " " <> show aMessage <> "\n")


handleRemoteMessage server dbType connectionString aMessage@(TerminateProcess message) = do
  liftIO $ debugMessage  $ pack ("Terminating self " <> show aMessage <> "\n")
  getSelfPid >>= flip exit (show aMessage)

handleRemoteMessage server dbType connectionString aMessage@(ComputeNodeEvent(mesageId, processId, rCommand)) = do 
  liftIO $ debugMessage $ pack ("Compute node event : " <> show rCommand)

handleRemoteMessage server dbType connectionString unhandledMessage =
  liftIO $ debugMessage  $ pack ("Received unhandled message  " <> show unhandledMessage <> "\n")

handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification server notificationMessage@(ProcessMonitorNotification _ pid _) = do
  liftIO $ debugMessage $ pack ("Monitor notification " <> show notificationMessage <> "\n")
  void $ liftIO $ atomically $ removeProcess server pid
  terminate


preludeEqual = (Prelude.==)
eventLoop :: ServerReaderT ()
eventLoop = do
  serverConfiguration <- ask
  Trans.lift $ do
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
        , matchIf (\(WhereIsReply l _) -> preludeEqual l sName) $
                handleWhereIsReply serverL Writer
        , matchAny $ \_ -> return ()      -- discard unknown messages
        ] `Control.Distributed.Process.catch` (\e@(SomeException e1) -> liftIO $ putStrLn $ "Exception " <> show e <> "\n")

computeService :: ServerReaderT ()
computeService = do
  initializeProcess
  eventLoop
  return()


