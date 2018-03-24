
module Data.Zya.Persistence.Persistence
  (
  DBType
  , persist
  , CreateStatus(..)
  , DBVendor(..)
  , DBType(..)
  , ConnectionDetails(..)
  , MessageT
  )
where
import Data.Text as Text
import Control.Monad.Reader
import Data.Zya.Persistence.Internal.Common as Common
import Data.Zya.Persistence.Internal.Postgres as Postgres
import Data.Zya.Persistence.Internal.Sqlite as Sqlite
import Control.Exception.Safe
{- |
  Persist a process message.
-}

persist :: MessageT
persist = do
  (dbTypeL, _,  _) <- ask
  case dbTypeL of
    RDBMS Postgresql -> Postgres.persistZ
    RDBMS Sqlite -> Sqlite.persistZ
    _ -> throw $ UnsupportedDBType dbTypeL

