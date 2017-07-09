{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.ServiceTypes where

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
subscriptionService :: String -> Process () 
subscriptionService aPort = return ()

type ServerReaderT = ReaderT (Server, Backend, ServiceProfile, ServiceName) Process
