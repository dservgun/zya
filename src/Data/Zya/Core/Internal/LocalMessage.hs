{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Zya.Core.Internal.LocalMessage
  (
    Command,
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

import Data.Time(UTCTime)
import Data.Text
import Data.Zya.Core.Internal.MessageDistribution
import Control.Distributed.Process
import Data.Aeson 
import GHC.Generics (Generic)




{--
  We use time stamp despite as a way to present some form of ordering. The values are at
  best approximate.
--}
data DeviceTimeStamp = DeviceTimeStamp{ _undevices :: (Device, UTCTime)} deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type Command = Text
-- The message id is unique among all the processes.
type MessageId = Text



data Device = Device {_undevice :: Text} deriving(Eq, Ord, Show, Generic, ToJSON, FromJSON)
data UserName = UserName {_unUserName :: Text} deriving (Show, Generic, ToJSON, FromJSON)


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


-- TODO: MessageId needs to be a newtype.
-- | Constructors.
createMessageSummaryP :: Text -> ProcessId -> LocalMessage
createMessageSummaryP messageIdL processIdL = createMessageSummary messageIdL $ pack . show $ processIdL

createMessageSummary :: Text -> Text -> LocalMessage
createMessageSummary messageIdL processIdL = MessageSummary "MessageSummary" messageIdL processIdL

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
login _ _  = undefined



