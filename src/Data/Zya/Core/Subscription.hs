{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Zya.Core.Subscription where
import Data.Text(pack, unpack, Text)
import Data.Monoid((<>))
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import Data.Time(UTCTime, getCurrentTime)
import System.Environment(getArgs)
import Control.Distributed.Process
import Control.Concurrent.STM
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)

import Control.Applicative((<$>))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Exception
import Data.Data
import Data.Typeable
import Data.Zya.Core.Service
import Control.Monad.Reader
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)






--------------Application types ---
type CommitOffset = Integer 
data User = User {
  login :: Login
  , topics :: [(Topic, CommitOffset)]
} deriving (Show, Typeable, Generic)
data OpenIdProvider = Google | Yahoo | Facebook | LinkedIn deriving (Show, Typeable, Generic)
{- | Email needs to be validated. TODO
-}
type Email = Text 
{- | Support for login based on the email id and the open id. 
-}
data Login = Login {
    email :: Email 
    , openId :: OpenIdProvider
} deriving (Show, Typeable, Generic)


type Start = Integer
type End = Integer 
data OffsetHint = Beginning | Latest | MessageRange (Start , End) deriving (Show, Typeable, Generic)

data Subscribe = 
  Subscribe {
    topic :: Topic
    , user :: User
    , reader :: OffsetHint
} deriving (Show, Typeable, Generic)

data PMessage
  = MsgServerInfo         Bool ProcessId [Subscribe]
  | MsgSend               Subscribe Text -- make this json.
  | ServiceAvailable ServiceProfile ProcessId -- Announce that the service is available on the said process id.
  deriving (Typeable, Generic)

instance Binary Login 
instance Binary OpenIdProvider
instance Binary User
instance Binary OffsetHint
instance Binary Subscribe
instance Binary PMessage



---------- Basic types  ----
type PageSize = Integer

type ServiceName = Text
type Topic = Text 
type Location = Integer
type ErrorCode = Text

data Message = Message (UTCTime, Text) deriving (Typeable, Show)
data Error =  Error (ErrorCode, Text) deriving (Typeable, Show) 
instance Exception Error

data StartUpException = StartUpException Text deriving (Typeable, Show)
instance Exception StartUpException
subscriptionService :: String -> Process () 
subscriptionService aPort = do 
  return ()

type ServerReaderT = ReaderT (Server, Backend, ServiceProfile, ServiceName) Process

writerService :: ServerReaderT ()
writerService = undefined 

readerService :: ServerReaderT ()
readerService = undefined 

databaseService :: ServerReaderT ()
databaseService = undefined 

webservice :: ServerReaderT ()
webservice = undefined


proxyProcess :: Server -> Process ()
proxyProcess (Server _ _ _ _ _ _ proxychan mProcessId) =  forever $ join $ liftIO $ atomically $ readTChan proxychan

handleRemoteMessage :: Server -> PMessage -> Process ()
handleRemoteMessage = undefined

handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification = undefined


handleWhereIsReply _ (WhereIsReply _ Nothing) = return ()
handleWhereIsReply server (WhereIsReply _ (Just pid)) =
  liftIO $ atomically $ do
    --clientmap <- readTVar clients
    -- send our own server info,and request a response:
    return ()
topicAllocationEventLoop :: ServerReaderT ()
topicAllocationEventLoop = do
  (server, backend, profile, serviceName) <- ask
  lift $ do 
    let sName = unpack serviceName
    spawnLocal (proxyProcess server)
    forever $
      receiveWait
        [ 
        match $ handleRemoteMessage server
        , match $ handleMonitorNotification server
        , matchIf (\(WhereIsReply l _) -> l == sName) $
                handleWhereIsReply server
        , matchAny $ \_ -> return ()      -- discard unknown messages
        ]

{- | 
  What does the allocator do: 
  It comes up and updates its local cache with its own
  process id as an allocator and starts a topic allocator event loop
  that listens to the any peers announcing themselves as allocators. If there is a 
  conflict, the allocator will switch itself as a backup and publish a message 
  announcing that. Will this work? 
  Note about failure and reliability: there is probably a need to implemmenting
  some form of consensus : raft seems the less daunting option. Implement the 
  c-interface to a reference implementation or complete the implementation using
  CH convenience functions. In our current implementation, we will skip this to 
  get a basic understanding of the overall interactions with the system. 

-}

topicAllocator :: ServerReaderT ()
topicAllocator = do 
  (server, backend, profile, serviceName) <- ask
  -- Convert a text to string.
  let serviceNameS = unpack serviceName
  mynode <- lift getSelfNode
  pid <- lift getSelfPid
  peers0 <- liftIO $ findPeers backend 1000000
  let peers = filter (/= mynode) peers0
  mypid <- lift getSelfPid
  lift $ register serviceNameS mypid
  forM_ peers $ \peer -> do
    lift $ whereisRemoteAsync peer serviceNameS
  topicAllocationEventLoop
  return ()
subscription :: Backend -> (ServiceProfile, Text) -> Process ()
subscription backend (sP, params) = do
  newServer <- newServer
  let readerParams = (newServer, backend, sP, params) 
  case sP of
    Writer -> runReaderT writerService readerParams
    Reader -> runReaderT readerService readerParams
    DatabaseServer -> runReaderT databaseService readerParams
    WebServer ->  runReaderT webservice readerParams
    TopicAllocator -> runReaderT topicAllocator readerParams


parseArgs :: IO (ServiceProfile, Text, String)
parseArgs = do
  [serviceName, lparams, portNumber] <- getArgs
  let params = pack lparams
  return $ 
    case serviceName of 
      "Writer" -> (Writer, params, portNumber)
      "Reader" -> (Reader, params, portNumber)
      "Database" -> (DatabaseServer, params, portNumber) 
      "Webserver" -> (WebServer, params, portNumber)
      "TopicAllocator" -> (TopicAllocator, params, portNumber)
      _  -> throw $ StartUpException $ pack $ "Invalid arguments " <> (serviceName) <> ":" <> lparams

remotable ['subscriptionService]


cloudEntryPoint :: Backend -> (ServiceProfile, ServiceName) -> IO ()
cloudEntryPoint backend (sP, sName) = do
  node <- newLocalNode backend 
  Node.runProcess node (subscription backend (sP, sName))


cloudMain :: IO () 
cloudMain = do 
 (sProfile, sName, aPort) <- parseArgs
 backend <- initializeBackend "localhost" aPort
                              ( __remoteTable initRemoteTable)
 cloudEntryPoint backend (sProfile, sName)
