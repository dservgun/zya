module Data.Zya.Persistence.PersistZ where
import Control.Monad.Reader
import Data.Zya.Core.ServiceTypes
import Data.Text


data DBVendor = Postgres
data DBType = FileSystem | RDBMS DBVendor 


{- |
	Convenience methods for some datatypes. 
-}

defaultPostgres :: DBType 
defaultPostgres = RDBMS Postgres

{- | 
 - A place to manage persistence.
-}
class PersistZ dbType where 
	persistZ :: ReaderT (dbType, PMessage) IO (Either Text PMessage)
