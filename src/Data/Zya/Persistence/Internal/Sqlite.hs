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
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Monad.Logger(runStderrLoggingT)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Data.ByteString.Char8
import Data.Text
import Data.Time
import Data.Zya.Core.Service
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

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
  internalPersist connStr message
  return $ CreateStatus $ Data.Text.pack $ show message


internalPersist :: (MonadBaseControl IO m, MonadIO m, Show a) => String -> a -> m  CreateStatus
internalPersist connStr message =
      runSqlite (Data.Text.pack connStr) $ do
        c <- ask
        runMigration migrateAll
        currentTime <- liftIO getCurrentTime
        messageId <- insert $ InMemoryMessage (Data.Text.pack $ show message) currentTime
        return $ CreateStatus $  Data.Text.pack $ show message
