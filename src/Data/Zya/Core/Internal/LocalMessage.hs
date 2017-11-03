{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Data.Zya.Core.Internal.LocalMessage where

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
data LocalMessage =
    Login{_kind :: Text, _userName :: UserName, _device :: Device, _timestamp :: UTCTime}
  | Logout {_kind :: Text, _userName :: UserName, _device :: Device, _timestamp :: UTCTime}
  | Session {_kind :: Text, userName :: UserName , device :: Device, _timestamp :: UTCTime, _topics :: [Topic]}
  | Topics {_kind :: Text, _userName :: UserName, _device :: Device, _timestamp :: UTCTime, topics :: [Topic]}
  | Publish{_kind :: Text, _userName :: UserName, _device :: Device, _timestamp :: UTCTime
                , topic :: Topic, messageId :: Text, messaggePayload :: Text}
  | Commit {_kind :: Text, userName :: UserName, device :: Device, topic :: Topic, messageId :: MessageId, timestamp :: UTCTime}
    deriving(Generic, ToJSON, FromJSON, Show)


instance Binary OpenIdProvider

