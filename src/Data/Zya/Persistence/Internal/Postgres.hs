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

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Logger(runStderrLoggingT)
import Data.Zya.Persistence.PersistZ
import Data.Bifunctor
import Database.Persist.TH 
import Database.Persist 
import Database.Persist.Postgresql
import Data.Time
import Data.Text
import Data.ByteString.Char8
import Data.Zya.Core.ServiceTypes



share [mkPersist sqlSettings, mkMigrate "migrateAll"] 
 [persistLowerCase|
    Message 
      text Text
      insertTime UTCTime default = CURRENT_TIMESTAMP
      deriving Show
|]  

c8Pack :: String -> ByteString
c8Pack = Data.ByteString.Char8.pack

instance PersistZ DBType where 
  persistZ = do 
    (dbType, ConnectionDetails connStr, message) <- ask
    -- save the message.
    let c = c8Pack connStr
    insert <- 
        runStderrLoggingT $ withPostgresqlPool c 10 $ \pool -> liftIO $ do
          flip runSqlPersistMPool pool $ do
            runMigration migrateAll
            currentTime <- liftIO getCurrentTime
            messageId <- insert $ Message (Data.Text.pack $ show message) currentTime
            return messageId
    return $ Right message

