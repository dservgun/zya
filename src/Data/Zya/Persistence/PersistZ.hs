module Data.Zya.Persistence.PersistZ where
import Control.Monad.Reader
import Data.Zya.Core.ServiceTypes
import Data.Text
import Data.Bifunctor




defaultDb :: DBType 
defaultDb = RDBMS Postgres


class PersistZ dbType where 
  persistZ :: ReaderT (dbType, ConnectionDetails, PMessage) IO (Either Text PMessage)
