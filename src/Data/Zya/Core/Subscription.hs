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
data Subscription = 
  Subscription {
    topic :: Topic
    , reader :: Reader
    , writer :: Writer
} deriving (Show)

type ReaderLocation = Integer -- for larger files
type WriterLocation = Integer
data WriteAudit = WriteAudit {
  writeLocation :: Location
  , lastWrite :: UTCTime
  , subscription :: Subscription
} deriving Show

data ReaderAudit = ReaderAudit {
  messages :: [Message]
  , pageSize :: PageSize
  , lastAccess :: UTCTime
} deriving Show
newtype PersistentStream = PersistentStream {uFP :: FilePath} deriving Show 
newtype ReaderStream = ReaderStream {un :: (ReaderLocation, [Message])} deriving Show 
newtype WriteStream = WriterStream {unW :: WriterLocation} deriving Show
data Reader = Reader {
  readStream :: ReaderStream
  , position :: ReaderLocation
  , fileName :: PersistentStream
  , audit :: ReaderAudit
} deriving Show 

data Writer = Writer {
    writeStream :: WriteStream
    , wFileName :: PersistentStream
    , writePosition :: WriterLocation
    , waudit :: WriteAudit
} deriving Show





---------- Basic types  ----
type PageSize = Integer

type Topic = Text 
type Location = Integer
type ErrorCode = Text

data Message = Message (UTCTime, Text) deriving (Typeable, Show)
data Error =  Error (ErrorCode, Text) deriving (Typeable, Show) 
instance Exception Error

