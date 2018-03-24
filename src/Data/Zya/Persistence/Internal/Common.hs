module Data.Zya.Persistence.Internal.Common 
  (DBVendor(..)
    , DBType (..)
    , ConnectionDetails(..)
    , CreateStatus(..)
    , UnsupportedDBType(..)
    , MessageT)
where 

import Control.Monad.Trans.Reader
import Control.Exception.Safe
import Data.Text
import Data.Zya.Core.Internal.ServerTypes

data DBVendor = Postgresql | Sqlite deriving(Show)
data DBType = FileSystem | RDBMS DBVendor deriving(Show)
newtype ConnectionDetails = ConnectionDetails {unStr :: String} deriving (Show)
newtype CreateStatus = CreateStatus {_un :: Text} deriving(Show)

newtype UnsupportedDBType = UnsupportedDBType {_undb :: DBType} deriving(Show)
instance Exception UnsupportedDBType

--- Database types
{-| Internal type for persisting process messages -}
type MessageT = ReaderT (DBType, ConnectionDetails, PMessage) IO CreateStatus
