{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- A webservice for the cloud.

module Data.Zya.Core.WebServerService
  (
    webService, startWebServer
  )
where



import Conduit
import Control.Concurrent.Async as Async(async)
import Control.Concurrent.STM.Lifted
import Control.Distributed.Process as Process
import Control.Exception
import Control.Lens
import Control.Monad (forever, void)
import Control.Monad.Catch as Catch
import Control.Monad.Trans.Reader
import Data.Monoid ((<>))
import Data.Map as Map
import Data.Text as Text (Text, unpack, pack)
import Data.Time
import Data.Zya.Core.Internal.ServerTypes
import Data.Zya.Core.Internal.WebserviceProtocolHandler
import Data.Zya.Core.Internal.LocalMessage
import Data.Zya.Core.Service
import Data.Zya.Persistence.Persistence(DBType, ConnectionDetails, MessageT)
import Data.Zya.Utils.Logger
import Network.WebSockets.Connection as WS (sendTextData, receiveData)
import Text.Printf
import Yesod.Core
import Yesod.WebSockets

newtype App = App Server

instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]


type YesodHandler = HandlerT App IO

app :: WebSocketsT (HandlerT App IO) ()
app = do
    connection <- ask
    App foundation <- getYesod
    -- get the last 20 pages.
    _ <- liftIO $ runReaderT (_runConn protocolHandler) (connection, foundation, Last (20, Page 10))
    return ()

getHomeR :: HandlerT App IO Text
getHomeR = do
  webSockets app
  return ("Done processing." :: Text)




publishToLocalQueuesIO :: (ProcessId, MessageId, Server) -> IO ()
publishToLocalQueuesIO = runBroadcastMessage publishToLocalQueues 

startWebServer :: ServerReaderT ()
startWebServer = do
  serverConfiguration <- ask

  lift $ liftIO $ debugMessage $ pack  "Starting webservice \n"
  _ <- liftIO $ putStrLn "Starting webservice\n"
  _ <- liftIO $ do
    let serverL = serverConfiguration^.server
    let webserverPortL = serverConfiguration^.webserverPort
    Async.async $ warp webserverPortL  $ App serverL
  lift $ liftIO $ debugMessage $ pack  "Webserver started\n"


handleRemoteMessage :: Server -> DBType -> ConnectionDetails -> Maybe Int -> PMessage -> Process ()
handleRemoteMessage serverL _ _ _ aMessage@(CreateTopic _)  = do
  liftIO $ debugMessage $ pack  ("Received message " <> show aMessage <> "\n")
  return ()


handleRemoteMessage serverL _ connectionString _ aMessage@(ServiceAvailable serviceProfileL pid) = do
  liftIO $ debugMessage $ pack  ("WebServer : Received Service Available message " <> show aMessage <> "\n")
  currentTime <- liftIO getCurrentTime
  _ <- liftIO $ atomically $ do
      myPid <- getMyPid serverL
      _ <- addService serverL serviceProfileL pid
      sendRemote serverL pid (GreetingsFrom WebServer myPid, currentTime)
  return ()

handleRemoteMessage serverL _ _ _ aMessage@(GreetingsFrom serviceProfileL pid) = do
  liftIO $ debugMessage $ pack  ("Received message " <> show aMessage <> "\n")
  _ <- liftIO $ atomically $ addService serverL serviceProfileL pid
  return ()

-- If the message has a tag, can cache and the server can also cache,
-- perhaps we can cache the message.
handleRemoteMessage serverL _ _ messageCount
  aMessage@(CommittedWriteMessage _ _) = 
    liftIO $ debugMessage $ pack  "Webserver ignoring committed write message.\n"

handleRemoteMessage serverL _ _ _ aMessage@(MessageKeyStore (messageId, processId)) = do
  _ <- liftIO $ atomically $ updateMessageKey serverL processId messageId
  _ <- liftIO $ publishToLocalQueuesIO (processId, messageId, serverL)
  liftIO $ debugMessage $ pack  "Message key store message processed..\n"
  return()

-- WriteMessage is not intended for a webserver service.
handleRemoteMessage _ _ _ _
  aMessage@(WriteMessage _ _ _) = do
  selfPid <- getSelfPid
  liftIO $ debugMessage $ pack  ("Received message " <> "Processor " <> show selfPid <> " " <> show aMessage <> "\n")
  return ()

handleRemoteMessage _ _ _ _ aMessage@(QueryMessage _) = do
  liftIO $ debugMessage $ pack  ("Received message " <> show aMessage <> "\n")
  return ()

handleRemoteMessage _ _ _ _ aMessage = do
  liftIO $ debugMessage $ pack  ("Terminating self " <> show aMessage <> "\n")
  getSelfPid >>= flip exit (show aMessage)

handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification serverL notificationMessage@(ProcessMonitorNotification _ pid _) = do
  liftIO $ debugMessage $ pack  ("Monitor notification " <> show notificationMessage <> "\n")
  void $ liftIO $ atomically $ removeProcess serverL pid
  terminate

eventLoop :: ServerReaderT ()
eventLoop = do
  serverConfiguration <- ask
  lift $ do
    let sName = Text.unpack $ serverConfiguration^.serviceName
    let serverL = serverConfiguration^.server
    let dbTypeL = serverConfiguration^.dbType
    let connectionDetailsL = serverConfiguration^.connDetails
    let testMessageCount = serverConfiguration^.numberOfTestMessages
    _ <- spawnLocal (proxyProcess serverL)
    forever
      (receiveWait
        [
        match $ handleRemoteMessage serverL dbTypeL connectionDetailsL testMessageCount
        , match $ handleMonitorNotification serverL
        , matchIf (\(WhereIsReply l _) -> l == sName) $
                handleWhereIsReply serverL WebServer
        , matchAny $ \_ -> return ()      -- discard unknown messages
        ]) `Process.catchExit`
              (\_ (TerminateProcess aText) -> do
                  liftIO $ debugMessage $ pack  ("Terminating process " <> show aText <> " " <> show sName <> "\n")
                  return ())


webService :: ServerReaderT ()
webService = do
  initializeProcess
  Catch.catch startWebServer 
    (\a@(SomeException _) -> lift $ liftIO $ debugMessage $ pack  $ "" <> show a <>"\n")
  lift $ liftIO $ debugMessage $ pack  "Calling event loop \n"
  eventLoop

  return()
