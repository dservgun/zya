
module Data.Zya.Persistence.Persistence
  (
  DBType
  , persist
  , CreateStatus(..)
  )
where
import Data.Text as Text
import Control.Monad.Reader
import Control.Exception.Safe
import Data.Zya.Persistence.Internal.Postgres as Postgres
import Data.Zya.Persistence.Internal.Sqlite as Sqlite
import Data.Zya.Core.Service

{- |
  Persist a process message.
-}

newtype UnsupportedDBType = UnsupportedDBType {_undb :: DBType} deriving(Show)
instance Exception UnsupportedDBType

persist :: MessageT
persist = do
  (dbTypeL, _,  _) <- ask
  case dbTypeL of
    RDBMS Postgresql -> Postgres.persistZ
    RDBMS Sqlite -> Sqlite.persistZ
    _ -> throw $ UnsupportedDBType dbTypeL

