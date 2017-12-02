{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}


module Data.Zya.Core.Internal.ServerTypes where 

import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Exception.Safe

import Data.Aeson
import Data.Map as Map
import Data.Text
import Data.Time(UTCTime)
import Data.Zya.Core.Internal.MessageDistribution
import GHC.Generics (Generic)
import Network.WebSockets as WS (Connection)

---------- Basic types  ----


{-- | The server unifies remote and local processes to manage logging messages.
    * localClients - For each client identifier, list of topics and their read positions.
    * remoteClients - For each process id the state of the topics.
    * localWriters - Write position for a topic.
    * remoteWriters - Write position for a topic.
    * services - A map of the services running on the network.
    * statistics - A map of requests and responses for the network.
    * proxyChannel - The single channel for inter process communication.
    * myProcessId :
    ** Note: There should exist only one write position per topic.
 -}


type Command = Text
-- The message id is unique among all the processes.
type MessageId = Text

data Publisher = Publisher {_unPublish :: Topic} deriving (Show, Typeable, Generic)

data PMessage =
  -- Returns a set of subscribers handled by a process.
  MsgServerInfo Bool ProcessId [Subscriber]
  -- * Notifies a subscriber of the next message.
  | NotifyMessage Subscriber (MessageId, Text)
  -- * Writes a message on a topic.
  | WriteMessage Publisher ProcessId (MessageId, Topic, Text)
  -- * Message Key store information.
  -- UTCTime should probably be replaced with a vector clock.
  | MessageKeyStore (MessageId, ProcessId)
  -- * Commits an offset read for a subscriber.
  | CommitMessage Subscriber (MessageId, Text) -- Commit needs to know about the id that needs to be committed.
  | CommittedWriteMessage Publisher (MessageId, Topic, Text)
  | CommitFailedMessage Publisher (MessageId, Topic, Text)
  -- * Announces that a current service profile is available on a node.
  | ServiceAvailable ServiceProfile ProcessId
  | TerminateProcess Text
  | CreateTopic Text
  -- * When a service becomes available, this message greets the service.
  | GreetingsFrom ServiceProfile ProcessId
  -- Send the message back to the process id
  | QueryMessage (MessageId, ProcessId, Maybe PMessage)
  | ComputeNodeEvent (MessageId, ProcessId, Text)
  deriving (Typeable, Generic, Show)

data FairnessStrategy = RoundRobin | FirstOne deriving (Show)


{--|
  An exception condition when process id was expected to be present.
--}
data MissingProcessException =
              MissingProcessException
                {_unProcess :: ProcessId
                , message :: Text} deriving (Show, Typeable)

data QueueNotFound =
  QueueNotFound {
      _messageIdQ :: MessageId
    ,_processId :: ProcessId
  } deriving (Show, Typeable)
instance Exception MissingProcessException
instance Exception QueueNotFound


type ServiceName = Text
type Location = Integer
type ErrorCode = Text

data Request = Request {unRequest :: Text} deriving Show
data Response = Response {unResponse :: Text} deriving Show
data ClientIdentifier = ClientIdentifier {unClid :: Text} deriving (Show, Ord, Eq)
data ClientState = ClientState {
    topicCS :: Topic
    , readPos :: Integer
} deriving(Show, Ord, Eq)


type Start = Integer
type End = Integer



data Message = Message (UTCTime, Text) deriving (Typeable, Show)
data Error =  Error (ErrorCode, Text) deriving (Typeable, Show)
instance Exception Error

data StartUpException = StartUpException {_text :: Text} deriving (Typeable, Show)
instance Exception StartUpException

--------------Application types ---

data OpenIdProvider = Google | Facebook | LinkedIn deriving (Show, Typeable, Generic, ToJSON, FromJSON)
{- | Email needs to be validated. TODO
-}

type Email = Text
{- | Support for login based on the email id and the open id.
-}



data Device = Device {_undevice :: Text} deriving(Eq, Ord, Show, Generic, ToJSON, FromJSON)
data UserName = UserName {_unUserName :: Text} deriving (Show, Generic, ToJSON, FromJSON)


{--
  We use time stamp despite as a way to present some form of ordering. The values are at
  best approximate.
--}
data DeviceTimeStamp = DeviceTimeStamp{ _undevices :: (Device, UTCTime)} deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
data LocalMessage =
    Login{_kind :: Text, _userName :: UserName, _devices :: [DeviceTimeStamp], _timestamp :: UTCTime}
  | Logout {_kind :: Text, _userName :: UserName, _device :: Device, _timestamp :: UTCTime}
  | Session {_kind :: Text, userName :: UserName , device :: Device, _timestamp :: UTCTime, _topics :: [Topic]}
  | Topics {_kind :: Text, _userName :: UserName, _device :: Device, _timestamp :: UTCTime, topics :: [Topic]}
  | Publish{_kind :: Text, _userName :: UserName, _device :: Device, _timestamp :: UTCTime
                , topic :: Topic, messageId :: Text, messaggePayload :: Text}
  | Commit {_kind :: Text, userName :: UserName, device :: Device, topic :: Topic, _messageId :: MessageId, timestamp :: UTCTime}
  | MessageSummary {_kind :: Text, _messageIdS :: Text, _processIdS :: Text}
    deriving(Generic, ToJSON, FromJSON, Show)


-- GreyLists have a smaller timeout. Cloud logical time equivalent to 60 seconds?
data GreyList = GreyList {_unGrey :: (UserName, Device)} deriving(Show, Generic, ToJSON, FromJSON)

-- Black list have a logical timeout of 24 hours.
data BlackList = BlackList {_unBlack :: (UserName, Device, UTCTime)} deriving(Show, Generic, ToJSON, FromJSON)



{-- | Supported services -}
data ServiceProfile =
    WebServer
    | Reader
    | Writer
    | QueryService
    | TopicAllocator
    -- * Terminate all processes. This may not be needed.
    | Terminator
    -- * A writer to test some messages to the system.
    | TestWriter
    | ComputeNode
    | Unknown
    deriving(Show, Generic, Typeable, Eq, Ord)

data Server = Server {
    -- Clients connected to webserver, term is being used to differentiate from remote clients.
    localClients :: TVar (Map ClientIdentifier [ClientState])
    -- A bounded queue per client identifier.
    , localTBQueue :: TVar (Map ClientIdentifier (WS.Connection, TBQueue LocalMessage))
    , remoteClients :: TVar (Map (ProcessId, ClientIdentifier) [ClientState])
    , localWriters :: TVar (Map Topic Integer)
    , remoteWriters :: TVar (Map (ProcessId, Topic) Integer)
    , remoteServiceList :: TVar (Map (ProcessId, ServiceProfile) UTCTime)
    ,  services :: TVar (Map (ProcessId, ServiceProfile) Integer)
    , statistics :: TVar (Map ProcessId ([Request], [Response]))
    , _proxyChannel :: TChan(Process())
    , _myProcessId :: TVar ProcessId
    , _messageKey :: TVar (Map MessageId ProcessId)
    -- The location of the message in the cluster of query services
    , _messageLocation :: TVar(Map MessageId ProcessId)
    -- Except for query services, the rest of the processes wont be populating
    -- this cache. This should be modeled as a service level cache,
    -- that each kind of service should handle.
    , _messageValues :: TVar(Map MessageId PMessage)
}


