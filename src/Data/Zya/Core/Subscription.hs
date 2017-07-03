{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Data.Zya.Core.Subscription where
import Data.Text
import Data.Monoid((<>))
import Data.Time(UTCTime, getCurrentTime)
import System.Environment(getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Applicative((<$>))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Data.Data
import Data.Typeable
import Data.Zya.Core.Service
import Control.Monad.Reader
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)






--------------Application types ---
type CommitOffset = Integer
data User = User {
  login :: Login
  , topics :: [(Topic, CommitOffset)]
} deriving Show
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
data OffsetHint = Beginning | Latest | MessageRange (Start , End) deriving (Show)
data Subscribe = 
  Subscribe {
    topic :: Topic
    , user :: User
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


{- | Given a service profile and its parameters, manage subscriptions.
-}

subscriptionService :: String -> Process () 
subscriptionService aPort = do 
  -- do something here.
  return ()

subscription :: Backend -> (ServiceProfile, String) -> Process ()
subscription aBackend (sP, port) = do 
  --subscriptionService port
  --runReaderT $ subscriptionMTL (aBackend, sP, port)
  return ()
parseArgs :: IO (ServiceProfile, String)
parseArgs = return (Writer, "test")

remotable ['subscriptionService]

-- Will look something like something
cloudMain :: IO () 
cloudMain = do 
 (sProfile, aPort) <- parseArgs
 backend <- initializeBackend "localhost" aPort
                              ( __remoteTable initRemoteTable)
 node <- newLocalNode backend
 Node.runProcess node (subscription backend (sProfile, aPort))

