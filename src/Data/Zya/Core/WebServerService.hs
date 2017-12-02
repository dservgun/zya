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
import Control.Exception

import Control.Monad.Catch as Catch
import Control.Concurrent.STM.Lifted
import Control.Distributed.Process as Process
import Control.Concurrent.Async as Async (waitSTM, wait, async, cancel, waitEither, waitBoth, waitAny
                        , concurrently,asyncThreadId)

import Control.Lens
import Control.Monad (forever, void)
import Control.Monad.Trans.Reader
import Data.Monoid ((<>))
import Data.Text as Text (Text, take)
import Data.Time
import Data.UUID.V1(nextUUID)
import Data.Zya.Core.Service
import Data.Zya.Core.ServiceTypes
import Data.Typeable
import Network.WebSockets.Connection as WS (Connection, sendTextData, receiveData)
import Text.Printf
import Yesod.Core
import Yesod.WebSockets
import Data.Zya.Core.Internal.WebserviceProtocolHandler
import qualified Data.Text as Text (pack, unpack)


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
    x <- liftIO $ runReaderT (_runConn protocolHandler) (connection, foundation, Last (20, Page 10))
    return ()

getHomeR :: HandlerT App IO Text
getHomeR = do
  webSockets app
  return ("Done processing." :: Text)


startWebServer :: ServerReaderT ()
startWebServer = do
  serverConfiguration <- ask

  lift $ say $ printf "Starting webservice \n"
  liftIO $ putStrLn "Starting webservice\n"
  liftIO $ do
    let sName = Text.unpack $ serverConfiguration^.serviceName
    let serverL = serverConfiguration^.server
    let webserverPortL = serverConfiguration^.webserverPort
    Async.async $ warp webserverPortL  $ App serverL
  lift $ say $ printf "Webserver started\n"


handleRemoteMessage :: Server -> DBType -> ConnectionDetails -> Maybe Int -> PMessage -> Process ()
handleRemoteMessage server dbType connectionString _ aMessage@(CreateTopic aTopic)  = do
  say $  printf ("Received message " <> show aMessage <> "\n")
  return ()


handleRemoteMessage server dbType connectionString _ aMessage@(ServiceAvailable serviceProfile pid) = do
  say $  printf ("WebServer : Received Service Available message " <> show aMessage <> "\n")
  currentTime <- liftIO getCurrentTime
  _ <- liftIO $ atomically $ do
      myPid <- getMyPid server
      addService server serviceProfile pid
      sendRemote server pid (GreetingsFrom WebServer myPid, currentTime)
  return ()

handleRemoteMessage server dbType connectionString _ aMessage@(GreetingsFrom serviceProfile pid) = do
  say $  printf ("Received message " <> show aMessage <> "\n")
  liftIO $ atomically $ addService server serviceProfile pid
  return ()

-- If the message has a tag, can cache and the server can also cache,
-- perhaps we can cache the message.
handleRemoteMessage server dbType connectionString messageCount
  aMessage@(CommittedWriteMessage publisher (messageId, topic, message)) = 
    say $ printf "Not handling this message. Not a writer.\n"

handleRemoteMessage server _ _ _ aMessage@(MessageKeyStore (messageId, processId)) = do
  myPid <- getSelfPid
  currentTime <- liftIO getCurrentTime
  liftIO $ atomically $ updateMessageKey server processId messageId
  _ <- liftIO $ sendWelcomeMessages (processId, messageId, server)
  say $ printf "Message key store message processed..\n"
  return()

handleRemoteMessage server dbType connectionString messageCount
  aMessage@(WriteMessage publisher processId (messageId, topic, message)) = do
  selfPid <- getSelfPid
  say $  printf ("Received message " <> "Processor " <> show selfPid <> " " <> show aMessage <> "\n")
  return ()


handleRemoteMessage server dbType connectionString _ aMessage@(QueryMessage (messageId, processId, message)) = do
  say $ printf ("Received message " <> show aMessage <> "\n")
  return ()


handleRemoteMessage server dbType connectionString _ aMessage@(TerminateProcess message) = do
  say $ printf ("Terminating self " <> show aMessage <> "\n")
  getSelfPid >>= flip exit (show aMessage)


handleRemoteMessage server dbType connectionString unhandledMessage _ =
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
    let sName = Text.unpack $ serverConfiguration^.serviceName
    let serverL = serverConfiguration^.server
    let profileL = serverConfiguration^.serviceProfile
    let dbTypeL = serverConfiguration^.dbType
    let connectionDetailsL = serverConfiguration^.connDetails
    let testMessageCount = serverConfiguration^.numberOfTestMessages
    spawnLocal (proxyProcess serverL)
    forever
      (receiveWait
        [
        match $ handleRemoteMessage serverL dbTypeL connectionDetailsL testMessageCount
        , match $ handleMonitorNotification serverL
        , matchIf (\(WhereIsReply l _) -> l == sName) $
                handleWhereIsReply serverL WebServer
        , matchAny $ \_ -> return ()      -- discard unknown messages
        ]) `Process.catchExit`
              (\pId (TerminateProcess aText) -> do
                  say $ printf ("Terminating process " <> show aText <> " " <> show sName <> "\n")
                  return ())

webService :: ServerReaderT ()
webService = do
  initializeProcess
  Catch.catch startWebServer (\a@(SomeException e) -> lift $ say $ printf $ "" <> show a <>"\n")

  lift $ say $ printf "Calling event loop \n"
  eventLoop

  return()
