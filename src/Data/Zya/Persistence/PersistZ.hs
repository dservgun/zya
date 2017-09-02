module Data.Zya.Persistence.PersistZ where
import Control.Monad.Reader
import Data.Zya.Core.ServiceTypes
import Data.Text
import Data.Bifunctor

data DBVendor = Postgres
data DBType = FileSystem | RDBMS DBVendor 
-- Name conflict, used to be ConnectionString.
newtype ConnectionDetails = ConnectionDetails {unStr :: String} deriving (Show)


defaultPostgres :: DBType 
defaultPostgres = RDBMS Postgres


class PersistZ dbType where 
  persistZ :: ReaderT (dbType, ConnectionDetails, PMessage) IO (Either Text PMessage)
