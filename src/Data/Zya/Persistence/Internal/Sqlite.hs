{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Zya.Persistence.Internal.Sqlite where 
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Logger(runStderrLoggingT)
import Data.Zya.Persistence.PersistZ
import Data.Bifunctor
import Database.Persist.TH 
import Database.Persist 
import Database.Persist.Sqlite
import Data.Time
import Data.Text
import Data.ByteString.Char8
import Data.Zya.Core.ServiceTypes


share [mkPersist sqlSettings, mkMigrate "migrateAll"] 
 [persistLowerCase|
    InMemoryMessage 
      text Text
      insertTime UTCTime default = CURRENT_TIMESTAMP
      deriving Show
|]  


c8Pack :: String -> ByteString
c8Pack = Data.ByteString.Char8.pack

persistZ :: MessageT
persistZ = do 
  (dbType, ConnectionDetails connStr, message) <- ask
  runSqlite (Data.Text.pack connStr) $ do
      runMigration migrateAll
      currentTime <- liftIO getCurrentTime
      messageId <- insert $ InMemoryMessage (Data.Text.pack $ show message) currentTime
      return messageId
  return $ CreateStatus $  Data.Text.pack $ show message

