{-# LANGUAGE DeriveDataTypeable #-}
module Data.Zya.Core.Subscription where
import Data.Text
import Data.Time(UTCTime, getCurrentTime)

import Control.Applicative((<$>))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Data.Data
import Data.Typeable




subscribe :: Subscription -> Reader 
subscribe = undefined

publish :: Subscription -> Writer
publish = undefined


writeMessage :: Writer -> Message -> IO Message
writeMessage w m = do
  ct <- liftIO $ (getCurrentTime :: IO UTCTime)
  throwM $ Error (pack "001", pack "error")


--------------Application types ---
data OpenIdProvider = Google | Yahoo | Facebook | LinkedIn deriving (Show)
{- | Email needs to be validated. TODO
-}
type Email = Text 
{- | Support for login based on the email id and the open id. 
-}
data Login = Login {
    email :: Email 
    , openId :: OpenIdProvider
} deriving (Show)


type Start = Integer
type End = Integer
data OffsetHint = Beginning | Latest | MessageRange of (Start , End)
data Subscribe = 
  Subscribe {
    topic :: Topic
    user :: User
    , reader :: OffsetHint
} deriving (Show)






---------- Basic types  ----
type PageSize = Integer

type Topic = Text 
type Location = Integer
type ErrorCode = Text

data Message = Message (UTCTime, Text) deriving (Typeable, Show)
data Error =  Error (ErrorCode, Text) deriving (Typeable, Show) 
instance Exception Error

