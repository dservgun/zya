
module Data.Zya.Persistence.Persistence
  (
  DBType
  , persist
  ) 
where
import Data.Text as Text
import Data.Zya.Core.ServiceTypes(PMessage)
import Data.Zya.Persistence.PersistZ as PersistZ
import Control.Monad.Reader
import Data.Zya.Persistence.Internal.Postgres
import Data.Zya.Core.ServiceTypes
{- | 
  Persist a process message to the appropriate database.
-}

type MessageT = ReaderT (DBType, ConnectionDetails, PMessage) IO (Either Text PMessage)

persist :: MessageT 
persist = PersistZ.persistZ 

