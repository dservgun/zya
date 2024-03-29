{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Zya.Persistence.Internal.Postgres where

import Control.Monad.IO.Class(liftIO)
import Control.Monad.Logger(runStderrLoggingT)
import Control.Monad.Trans.Reader
import Data.ByteString.Char8
import Data.Monoid((<>))
import Data.Text
import Data.Time
import Data.Zya.Persistence.Internal.Common
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
 [persistLowerCase|
    Message
      text Text
      insertTime UTCTime default = CURRENT_TIMESTAMP
      deriving Show
|]

c8Pack :: String -> ByteString
c8Pack = Data.ByteString.Char8.pack

persistZ :: MessageT
persistZ = do
  (_, ConnectionDetails connStr, message) <- ask
  -- save the message.
  let c = c8Pack connStr
  insertResult <-
      runStderrLoggingT $ withPostgresqlPool c 10 $ \pool -> liftIO $
        flip runSqlPersistMPool pool $ do
          runMigration migrateAll
          currentTime <- liftIO getCurrentTime
          insert $ Message (Data.Text.pack $ show message) currentTime
  return $ CreateStatus $ Data.Text.pack $ show message <> " " <> show insertResult

