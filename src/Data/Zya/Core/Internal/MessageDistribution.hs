{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Data.Zya.Core.Internal.MessageDistribution where


import Control.Applicative((<$>))
import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.List as List
import Data.Map.Strict as Map
import Data.Monoid((<>))
import Data.Text(unpack, Text)
import Data.Time(UTCTime, getCurrentTime)
import Data.Typeable
import Data.Zya.Utils.Logger(debugMessage)
import Data.Aeson
import GHC.Generics (Generic)
import Network.WebSockets.Connection as WS (Connection)
import Text.Printf
import Data.Binary

type CommitOffset = Integer
newtype Topic = Topic {unTopic :: Text} deriving (Show, Ord, Eq, Generic, ToJSON, FromJSON)

{-- |
  Begin -> All messages in some order, need not be timeordered.
  End -> All new messages from now.
  Last n -> A few messages to get some context on the topic.
--}
data MessageDistributionStrategy = Begin | End | Last (Int, Page) deriving (Typeable, Show, Generic, ToJSON, FromJSON)
newtype Page = Page {_uInt :: Int} deriving(Typeable, Show, Generic, ToJSON, FromJSON)

type User = Text
data Subscriber =
  Subscriber {
    topic :: Topic
    , user :: User
    , readerStrategy :: MessageDistributionStrategy
} deriving (Show, Typeable, Generic, ToJSON, FromJSON)

instance Binary MessageDistributionStrategy
instance Binary Page
instance Binary Topic
instance Binary Subscriber
