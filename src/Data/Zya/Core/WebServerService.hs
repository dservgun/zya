{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies #-}


-- A webservice for the cloud. 

module Data.Zya.Core.WebServerService
  (
    webService, getHomeR
  )
where 


import Conduit
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.Lifted
import Control.Distributed.Process as Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Debug(traceOn, systemLoggerTracer, logfileTracer,traceLog)
import Control.Distributed.Process.Node as Node hiding (newLocalNode)
import Control.Lens
import Control.Monad (forever, void)
import Control.Monad.Trans.Reader
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time
import Data.Zya.Core.Service
import Data.Zya.Core.ServiceTypes
import Network.WebSockets.Connection as WS (Connection, sendTextData)
import qualified Data.Text as Text (pack, unpack)
import Text.Printf
import Yesod.Core
import Yesod.WebSockets
import Control.Monad.Logger
data App = App Server

instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]


type YesodHandler = HandlerT App IO

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
readerThread :: ProtocolHandler () 
readerThread = do
  (conn, app) <- ProtoHandler ask
  liftIO $ WS.sendTextData conn ("hello world" :: Text)
  return ()
protocolHandler :: ProtocolHandler WS.Connection
protocolHandler = do 
  (conn, app) <- ProtoHandler ask 
  return conn

app :: WebSocketsT YesodHandler ()
app = do
    connection <- ask 
    App foundation <- getYesod
    x <- liftIO $ runReaderT (_runConn protocolHandler) (connection, foundation)
    return ()
getHomeR :: HandlerT App IO Text
getHomeR = 
  return ("Done processing." :: Text)


startWebServer :: ServerReaderT ()
startWebServer = do 
  serverConfiguration <- ask
  liftIO $ do 
    let sName = Text.unpack $ serverConfiguration^.serviceName
    let serverL = serverConfiguration^.server 
    warp 3000 $ App serverL

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
      sendRemote server pid $ (GreetingsFrom WebServer myPid, currentTime)
  return ()

handleRemoteMessage server dbType connectionString _ aMessage@(GreetingsFrom serviceProfile pid) = do
  say $  printf ("Not Received message " <> (show aMessage) <> "\n")
  return ()

handleRemoteMessage server dbType connectionString messageCount 
  aMessage@(CommittedWriteMessage publisher (messageId, topic, message)) = do 
    say $ printf (" Not Handling remote message " <> show aMessage)
    

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
  startWebServer
  initializeProcess
  eventLoop

  return()
