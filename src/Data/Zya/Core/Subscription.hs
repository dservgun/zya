{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.Subscription where

import GHC.Generics (Generic)
import System.Environment(getArgs)

import Control.Concurrent.STM
import Control.Applicative((<$>))
import Control.Exception

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Reader

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)

import Data.Binary
import Data.Data
import Data.Monoid((<>))
import Data.Text(pack, unpack, Text)
import Data.Time(UTCTime, getCurrentTime)
import Data.Typeable
import Data.Zya.Core.Service




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
  | TerminateProcess Text
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

newtype Message = Message (UTCTime, Text) deriving (Typeable, Show)
newtype Error =  Error (ErrorCode, Text) deriving (Typeable, Show) 
instance Exception Error

newtype StartUpException = StartUpException Text deriving (Typeable, Show)
instance Exception StartUpException
subscriptionService :: String -> Process () 
subscriptionService aPort = return ()

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
proxyProcess server 
  =  forever $ join $ liftIO $ atomically $ readTChan $ proxyChannel server

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
    liftIO $ atomically $ do 
      selfPid <- readTVar $ myProcessId server
      case selfPid of 
        Just x -> updateTopicAllocator server x TopicAllocator
        Nothing -> return ()
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
  peers0 <- liftIO $ findPeers backend 1000000
  let peers = filter (/= mynode) peers0
  mypid <- lift getSelfPid
  lift $ register serviceNameS mypid
  forM_ peers $ \peer -> lift $ whereisRemoteAsync peer serviceNameS
  liftIO $ atomically $ do 
    updateSelfPid server mypid
    updateTopicAllocator server mypid TopicAllocator
  topicAllocationEventLoop
  return ()

{- | Terminate all processes calling exit on each -}
terminator :: ServerReaderT () 
terminator = do 
  (server, backend, profile, serviceName) <- ask
  remoteProcesses <- liftIO $ atomically $ remoteProcesses server
  lift $ do
    forM_ remoteProcesses $ \peer -> exit peer $ TerminateProcess "Shutting down the cloud"
    pid <- getSelfPid -- the state is not update in the terminator, at least for now.
    exit pid $ TerminateProcess "Shutting down self"

subscription :: Backend -> (ServiceProfile, Text) -> Process ()
subscription backend (sP, params) = do
  n <- newServer -- shadowing the one from io
  let readerParams = (n, backend, sP, params) 
  say $ "Starting subscrpition " <> (show sP) <> (show params)
  case sP of
    Writer -> runReaderT writerService readerParams
    Reader -> runReaderT readerService readerParams
    DatabaseServer -> runReaderT databaseService readerParams
    WebServer ->  runReaderT webservice readerParams
    TopicAllocator -> runReaderT topicAllocator readerParams
    Terminator -> runReaderT terminator readerParams


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
      "Terminator" -> (Terminator, params, portNumber)
      _  -> throw $ StartUpException $ pack $ "Invalid arguments " <> serviceName <> ":" <> lparams


remotable ['subscriptionService]


simpleBackend :: String -> String -> IO Backend 
simpleBackend = \a p -> initializeBackend a p $ Data.Zya.Core.Subscription.__remoteTable initRemoteTable

-- | For  example 'cloudEntryPoint (simpleBackend "localhost" "50000") (TopicAllocator, "ZYA")  '
cloudEntryPoint :: Backend -> (ServiceProfile, ServiceName) -> IO ()
cloudEntryPoint backend (sP, sName)= do
  node <- newLocalNode backend 
  Node.runProcess node (subscription backend (sP, sName))


cloudMain :: IO () 
cloudMain = do 
 (sProfile, sName, aPort) <- parseArgs
 backend <- simpleBackend "localhost" aPort
 cloudEntryPoint backend (sProfile, sName)
