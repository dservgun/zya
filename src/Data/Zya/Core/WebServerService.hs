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

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.Lifted
import Control.Distributed.Process as Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Debug(traceOn, systemLoggerTracer, logfileTracer,traceLog)
import Control.Distributed.Process.Node as Node hiding (newLocalNode)
import Control.Concurrent.Async as Async (waitSTM, wait, async, cancel, waitEither, waitBoth, waitAny
                        , concurrently,asyncThreadId)

import Control.Lens
import Control.Monad (forever, void)
import Control.Monad.Catch as Catch
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time
import Data.UUID.V1(nextUUID)
import Data.Zya.Core.Service
import Data.Zya.Core.ServiceTypes
import Network.WebSockets.Connection as WS (Connection, sendTextData, receiveData)
import Text.Printf
import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text as Text (pack, unpack)


data App = App Server

instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]



type Connection = Text
newtype ProtocolHandler a = 
      ProtoHandler {
        _runConn :: ReaderT (WS.Connection, Server) IO a
      }
      deriving 
      (
        Functor, 
        Applicative,
        Monad,
        MonadIO)


-- At this point we can safely be in the io monad, though adding a monad logger might
-- be beneficial.
-- readerThread :: ProtocolHandler () 
readerThread (conn, app) = do
  liftIO $ putStrLn "Reader thread."
  WS.sendTextData conn ("hello world\n" :: Text)
--    ($(logDebug) "Test")
  liftIO $ threadDelay (10 ^ 6 * 3)
  readerThread (conn, app)

writerThread (conn, app) = do  
  liftIO $ putStrLn "Writer thread"
  (command  :: Text ) <- liftIO $ (WS.receiveData conn)
  writerThread (conn, app)

removeConn :: ProtocolHandler WS.Connection 
removeConn = do 
  (conn, app) <- ProtoHandler ask 
  return conn
addConn :: ProtocolHandler WS.Connection 
addConn = do 
  (conn, app) <- ProtoHandler ask 
  nextId <- (\x -> Text.pack $ show x) <$> liftIO nextUUID
  r <- liftIO $ atomically $ addConnection app (ClientIdentifier nextId) conn 
  return conn
protocolHandler :: ProtocolHandler WS.Connection
protocolHandler = do 
  (conn, app) <- ProtoHandler ask 
  addConn
  a <- liftIO . liftIO $ Async.async (readerThread (conn, app))
  b <- liftIO . liftIO $ Async.async (writerThread (conn, app))
--  b <- Async.async $ liftIO writerThread 
  liftIO $ Async.waitAny [a, b]
  return conn

type YesodHandler = HandlerT App IO

app :: WebSocketsT (HandlerT App IO) ()
app = do
    connection <- ask 
    App foundation <- getYesod
    x <- liftIO $ runReaderT (_runConn protocolHandler) (connection, foundation)
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
  say $  printf ("Received message " <> (show aMessage) <> "\n")
  return ()


handleRemoteMessage server dbType connectionString _ aMessage@(ServiceAvailable serviceProfile pid) = do
  say $  printf ("WebServer : Received Service Available message " <> (show aMessage) <> "\n")  
  currentTime <- liftIO $ getCurrentTime
  _ <- liftIO $ atomically $ do 
      myPid <- getMyPid server
      addService server serviceProfile pid
      sendRemote server pid $ (GreetingsFrom WebServer myPid, currentTime)
  return ()

handleRemoteMessage server dbType connectionString _ aMessage@(GreetingsFrom serviceProfile pid) = do
  say $  printf ("Not Received message " <> (show aMessage) <> "\n")
  return ()

handleRemoteMessage server dbType connectionString messageCount 
  aMessage@(CommittedWriteMessage publisher (messageId, topic, message)) = do 
    say $ printf (" Not Handling remote message " <> show aMessage <> "\n")
    

handleRemoteMessage server dbType connectionString messageCount
  aMessage@(WriteMessage publisher (messageId, topic, message)) = do
  selfPid <- getSelfPid
  say $  printf ("Received message " <> "Processor " <> (show selfPid) <> " " <> (show aMessage) <> "\n")
  return ()


handleRemoteMessage server dbType connectionString _ aMessage@(QueryMessage (messageId, processId, message)) = do
  say $ printf ("Received message " <> show aMessage <> "\n") 
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
    let sName = Text.unpack $ serverConfiguration^.serviceName
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
