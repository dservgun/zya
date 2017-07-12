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
  ) where

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
  | CreateTopic Text 
  deriving (Typeable, Generic, Show)

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



startupException :: Text -> StartUpException
startupException = StartUpException

type ServerReaderT = ReaderT (Server, Backend, ServiceProfile, ServiceName) Process

subscriptionService :: String -> Process () 
subscriptionService aPort = return ()

sendRemote :: Server -> ProcessId -> PMessage -> STM ()
sendRemote aServer pid pmsg = writeTChan (proxyChannel aServer) (send pid pmsg)

initializeProcess :: ServerReaderT()
initializeProcess = do 
  (server, backend, profile, serviceName) <- ask
  let serviceNameS = unpack serviceName
  mynode <- lift getSelfNode
  peers0 <- liftIO $ findPeers backend peerTimeout
  let peers = filter (/= mynode) peers0
  mypid <- lift getSelfPid
  lift $ register serviceNameS mypid
  forM_ peers $ \peer -> lift $ whereisRemoteAsync peer serviceNameS
  liftIO $ atomically $ do 
    updateMyPid server mypid

proxyProcess :: Server -> Process ()
proxyProcess server 
  =  forever $ join $ liftIO $ atomically $ readTChan $ proxyChannel server
