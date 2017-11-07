{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.Zya.Core.Internal.LocalMessage
  (
    createMessageSummary
    , createMessageSummaryP
    , MessageId
    , Device
    , UserName
    , DeviceTimeStamp
    , LocalMessage(..)
    , commitMessage
  )

where

{-- |
  External messages that are supported by the cloud. Within the cloud messages use
  PMessage. Use LocalMessage when integrating an external client (not a member of the cloud)
--}

import Data.Aeson
import Data.HashMap.Strict as HM
import Data.Time(UTCTime)
import Data.Binary
import GHC.Generics
import Data.Text
import Data.Typeable
import Data.Zya.Core.Internal.MessageDistribution
import Control.Distributed.Process
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
--------------Application types ---

data OpenIdProvider = Google | Facebook | LinkedIn deriving (Show, Typeable, Generic, ToJSON, FromJSON)
{- | Email needs to be validated. TODO
-}

type Email = Text
{- | Support for login based on the email id and the open id.
-}



newtype Device = Device {_undevice :: Text} deriving(Show, Generic, ToJSON, FromJSON)
newtype UserName = UserName {_unUserName :: Text} deriving (Show, Generic, ToJSON, FromJSON)

-- The message id is unique among all the processes.
type MessageId = Text


{--
  We use time stamp despite as a way to present some form of ordering. The values are at
  best approximate.
--}
newtype DeviceTimeStamp = DeviceTimeStamp{ _undevices :: (Device, UTCTime)} deriving (Show, Generic, ToJSON, FromJSON)
data LocalMessage =
    Login{_kind :: Text, _userName :: UserName, _devices :: [DeviceTimeStamp], _timestamp :: UTCTime}
  | Logout {_kind :: Text, _userName :: UserName, _device :: Device, _timestamp :: UTCTime}
  | Session {_kind :: Text, userName :: UserName , device :: Device, _timestamp :: UTCTime, _topics :: [Topic]}
  | Topics {_kind :: Text, _userName :: UserName, _device :: Device, _timestamp :: UTCTime, topics :: [Topic]}
  | Publish{_kind :: Text, _userName :: UserName, _device :: Device, _timestamp :: UTCTime
                , topic :: Topic, messageId :: Text, messaggePayload :: Text}
  | Commit {_kind :: Text, userName :: UserName, device :: Device, topic :: Topic, _messageId :: MessageId, timestamp :: UTCTime}
  | MessageSummary {_kind :: Text, _messageId :: Text, _processId :: Text}
    deriving(Generic, ToJSON, FromJSON, Show)


-- GreyLists have a smaller timeout. Cloud logical time equivalent to 60 seconds?
newtype GreyList = GreyList {_unGrey :: (UserName, Device)} deriving(Show, Generic, ToJSON, FromJSON)

-- Black list have a logical timeout of 24 hours.
newtype BlackList = BlackList {_unBlack :: (UserName, Device, UTCTime)} deriving(Show, Generic, ToJSON, FromJSON)


-- | Constructors.
createMessageSummaryP :: Text -> ProcessId -> LocalMessage
createMessageSummaryP messageId processIdL = createMessageSummary messageId $ pack . show $ processIdL

createMessageSummary :: Text -> Text -> LocalMessage
createMessageSummary messageId processId = MessageSummary "MessageSummary" messageId processId

{-- |
  Commit a message: update the read status of the message in the store. Replicate
  the status all members in the cloud.
--}
commitMessage :: UserName -> Device -> Topic -> MessageId -> UTCTime -> LocalMessage
commitMessage aUserName aDevice aTopic aMessageId timeStamp =
    Commit "Commit" aUserName aDevice aTopic aMessageId timeStamp

{-- |
  Login the user into the cloud. Check if the user exists in the cloud then ensure
    that the device is not already logged in. If the (user, device) pair is already
    logged in, send an error back and update a global greylist.
    If the device doesnt exist, then add the device and let the cloud know.
    Update any grey lists for the username device pair across the cloud.
--}
login :: UserName -> Device -> LocalMessage
login aUser aDevice = undefined



{-newtype ProtocolHandler a =
      ProtoHandler {
        _runConn :: ReaderT (WS.Connection, Server, MessageDistributionStrategy) IO a
      }
      deriving
      (
        Functor,
        Applicative,
        Monad,
        MonadIO)
-}

type Command = Text
type Server = Text
newtype LocalMessageHandler a =
    LocalMessageHandler {
      _runHandler :: ReaderT (Command, ProcessId, Server, MessageDistributionStrategy) (StateT [Command] IO) a
    } deriving(Functor, Applicative, Monad, MonadIO)



instance Binary OpenIdProvider


