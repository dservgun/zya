
module Data.Zya.Persistence.Persistence
  (
  DBType
  , persist
  , CreateStatus(..)
  ) 
where
import Data.Text as Text
import Data.Zya.Core.ServiceTypes(PMessage)
import Control.Monad.Reader
import Data.Zya.Persistence.Internal.Postgres as Postgres
import Data.Zya.Persistence.Internal.Sqlite as Sqlite
import Data.Zya.Core.ServiceTypes

{- | 
  Persist a process message to the appropriate database.
-}


persist :: MessageT 
persist = do 
  (dbType, _,  _) <- ask
  case dbType of 
    RDBMS Postgresql -> Postgres.persistZ
    RDBMS Sqlite -> Sqlite.persistZ

