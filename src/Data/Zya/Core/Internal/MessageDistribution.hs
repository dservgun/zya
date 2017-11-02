module Data.Zya.Core.Internal.MessageDistribution where


import Control.Applicative((<$>))
import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Binary
import Data.List as List
import Data.Map.Strict as Map
import Data.Monoid((<>))
import Data.Text(unpack, Text)
import Data.Time(UTCTime, getCurrentTime)
import Data.Typeable
import Data.Zya.Utils.Logger(debugMessage)
import GHC.Generics (Generic)
import Network.WebSockets.Connection as WS (Connection)
import Text.Printf

{-- |
  Begin -> All messages in some order, need not be timeordered.
  End -> All new messages from now.
  Last n -> A few messages to get some context on the topic.
--}
newtype Page = Page {_uInt :: Int} deriving(Typeable, Show)
data MessageDistributionStrategy = Begin | End | Last (Int, Page) deriving (Typeable, Show)
