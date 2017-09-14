{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.ServiceTypes(
    -- * server reader 
    ServerReaderT
    -- * Message types.
    , PMessage(..)
    -- ** Some constants.
    , peerTimeout
    -- * Sending and receiving messages
    , sendRemote
    -- * Initializing the cloud process
    , initializeProcess
    , subscriptionService
    , ServiceName
    -- ** Exceptions and constructors
    , StartUpException
    , startupException
    -- ** Some utility functions
    , proxyProcess
    -- * Database constants
    , DBType(..)
    , ConnectionDetails(..)
    , DBVendor(..)
    , MessageT(..)
    , CreateStatus(..)
    -- * Server configuration 
    , ServerConfiguration
    , server, backend, serviceProfile  
    , serviceName, dbType, connDetails
    , makeServerConfiguration
    -- * Publisher details 
    , Publisher(..)
    -- * Some common handlers for all nodes
    , handleWhereIsReply
  ) where

import GHC.Generics (Generic)
import System.Environment(getArgs)

import Control.Concurrent.STM
import Control.Applicative((<$>))
import Control.Exception

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Reader

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)
import Control.Lens

import Data.Binary
import Data.Data
import Data.Monoid((<>))
import Data.Text(pack, unpack, take, Text)
import Data.Time(UTCTime, getCurrentTime)
import Data.Typeable
import Data.Zya.Core.Service
import Text.Printf


------------ Constants --------------
peerTimeout :: Int
peerTimeout = 1000000


--------------Application types ---
type CommitOffset = Integer 
data User = User {
  login :: Login
  , topics :: [(Topic, CommitOffset)]
} deriving (Show, Typeable, Generic)
data OpenIdProvider = Google | Facebook | LinkedIn deriving (Show, Typeable, Generic)
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

-- The message id is unique among all the processes.
type MessageId = Integer
data OffsetHint = Beginning | Latest | MessageRange (Start , End) deriving (Show, Typeable, Generic)

data Subscriber = 
  Subscriber {
    topic :: Topic
    , user :: User
    , reader :: OffsetHint
} deriving (Show, Typeable, Generic)

data Publisher = Publisher {unTopic :: Topic} deriving (Show, Typeable, Generic)

data PMessage = 
  -- Returns a set of subscribers handled by a process.
  MsgServerInfo         Bool ProcessId [Subscriber]
  -- * Notifies a subscriber of the next message.
  | NotifyMessage Subscriber (MessageId, Text)
  -- * Writes a message on a topic. 
  | WriteMessage Publisher (MessageId, Text)
  -- * Commits an offset read for a subscriber.
  | CommitMessage Subscriber (MessageId, Text) -- Commit needs to know about the id that needs to be committed.
  -- * Announces that a current service profile is available on a node.
  | ServiceAvailable ServiceProfile ProcessId 
  | TerminateProcess Text
  | CreateTopic Text 
  deriving (Typeable, Generic)

--MAX_BYTES :: Integer 
maxBytes = 10 * 1024 * 1024 * 1024 -- 

trim :: Int -> Text -> Text 
trim = Data.Text.take
instance Show PMessage where 
  show pMessage = 
      case pMessage of 
        MsgServerInfo a b l -> printf "MsgServerInfo %s %s %s" (show a) (show b) (show l)
        NotifyMessage s (m, t) -> printf "Notify message %s %d %s" (show s) (show m) (unpack $ trim maxBytes t) 
        WriteMessage p (m, t) -> printf "WriteMessage %s %d %s" (show p) m (unpack $ trim maxBytes t)
        CommitMessage s (m, t) -> printf "Commit message %s %d %s" (show s) m (unpack $ trim maxBytes t)
        ServiceAvailable s p -> printf "ServiceAvailable %s %s" (show s) (show p) 
        TerminateProcess s  -> printf "TerminateProcess %s" (show s) 
        CreateTopic t -> printf "CreateTopic %s" (show . unpack $ trim maxBytes t)

instance Binary Login 
instance Binary OpenIdProvider
instance Binary User
instance Binary OffsetHint
instance Binary Subscriber
instance Binary Publisher
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


--- Database types
data DBVendor = Postgresql | Sqlite
data DBType = FileSystem | RDBMS DBVendor 
newtype ConnectionDetails = ConnectionDetails {unStr :: String} deriving (Show)
newtype CreateStatus = CreateStatus {_un :: Text} deriving(Show)
{-| Internal type for persisting process messages -}
type MessageT = ReaderT (DBType, ConnectionDetails, PMessage) IO CreateStatus



startupException :: Text -> StartUpException
startupException = StartUpException

data ServerConfiguration = ServerConfig{
   _server :: Server 
  , _backend :: Backend 
  , _serviceProfile :: ServiceProfile 
  , _serviceName :: ServiceName 
  , _dbType :: DBType 
  , _connDetails :: ConnectionDetails
  } 

makeLenses ''ServerConfiguration

instance Show ServerConfiguration where 
  show s = printf "%s : %s" (show (s^.serviceProfile)) (show (s^.serviceName))

type ServerReaderT = ReaderT ServerConfiguration Process

makeServerConfiguration :: Server -> Backend -> ServiceProfile -> ServiceName -> DBType -> ConnectionDetails -> ServerConfiguration
makeServerConfiguration s b sp sName db cd = ServerConfig s b sp sName db cd
subscriptionService :: String -> Process () 
subscriptionService aPort = return ()

sendRemote :: Server -> ProcessId -> PMessage -> STM ()
sendRemote aServer pid pmsg = writeTChan (proxyChannel aServer) (send pid pmsg)


initializeProcess :: ServerReaderT()
initializeProcess = do 
  serverConfiguration <- ask
  let server1 = view server serverConfiguration
  let serviceName1 = view serviceName serverConfiguration
  let serviceNameS = unpack serviceName1
  let backendl = view backend serverConfiguration
  mynode <- lift getSelfNode

  peers0 <- liftIO $ findPeers backendl peerTimeout
  let peers = filter (/= mynode) peers0
  mypid <- lift getSelfPid
  lift $ register serviceNameS mypid
  forM_ peers $ \peer -> lift $ whereisRemoteAsync peer serviceNameS
  liftIO $ atomically $ do 
    updateMyPid server1 mypid


proxyProcess :: Server -> Process ()
proxyProcess server 
  =  forever $ join $ liftIO $ atomically $ readTChan $ proxyChannel server


-- Announce that a service has come up. 
-- When a service receives this message, it needs to send some info 
-- about itself to the new service. Will this result in n squared messages.

handleWhereIsReply :: Server -> ServiceProfile -> WhereIsReply -> Process ()
handleWhereIsReply server serviceProfile (WhereIsReply _ (Just pid)) = do
  mSpid <- 
    liftIO $ atomically $ do
    mySpId <- readTVar $ myProcessId server
    sendRemote server pid $ ServiceAvailable serviceProfile mySpId
    return mySpId
  say $ printf "Sending info about self %s -> %s, %s " (show mSpid) (show pid) (show serviceProfile)
handleWhereIsReply _ serviceProfile (WhereIsReply _ Nothing) = return ()
