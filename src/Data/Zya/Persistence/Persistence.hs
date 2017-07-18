
module Data.Zya.Persistence.Persistence
	(
	DBType
	, defaultPostgres
	, persistZ
	) 
where
import Data.Text as Text
import Data.Zya.Core.ServiceTypes(PMessage)
import Data.Zya.Persistence.Internal.Postgres as Postgres (persist) 
import Data.Zya.Persistence.Internal.FileSystem as FileSystem (persist)


data DBVendor = Postgres
data DBType = FileSystem | RDBMS DBVendor 


{- |
	Convenience methods for some datatypes. 
-}

defaultPostgres :: DBType 
defaultPostgres = RDBMS Postgres
{- | 
	Persist a process message to the appropriate database.
-}
persistZ :: DBType -> PMessage -> IO (Either Text PMessage)
persistZ FileSystem message = undefined 
persistZ defaultPostgres message = undefined