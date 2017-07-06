{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Zya.Core.Subscription where
import Data.Text(pack, unpack, Text)
import Data.Monoid((<>))
import Data.Time(UTCTime, getCurrentTime)
import System.Environment(getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)

import Control.Applicative((<$>))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Exception
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

data StartUpException = StartUpException Text deriving (Typeable, Show)
instance Exception StartUpException
subscriptionService :: String -> Process () 
subscriptionService aPort = do 
  return ()

type ServerReaderT = ReaderT (Server, Backend, ServiceProfile, Text) Process
writerService :: ServerReaderT ()
writerService = undefined 

readerService :: ServerReaderT ()
readerService = undefined 

databaseService :: ServerReaderT ()
databaseService = undefined 

webservice :: ServerReaderT ()
webservice = undefined


-- TODO: Change it to using MonadReader: fundep issue.
{- | 
  What does the allocator do: 
  It comes up and updates its local cache with its own
  process id as an allocator and starts a topic allocator event loop
  that listens to the any peers announcing themselves as allocators. If there is a 
  conflict, the allocator will switch itself as a backup and publish a message 
  announcing that. Will this work?
-}
topicAllocator :: ServerReaderT ()
topicAllocator = do 
  (server, backend, profile, params) <- ask
  mynode <- lift getSelfNode
  pid <- lift getSelfPid
  peers0 <- liftIO $ findPeers backend 1000000
  let peers = filter (/= mynode) peers0

  return ()
subscription :: Backend -> (ServiceProfile, Text) -> Process ()
subscription backend (sP, params) = do
  newServer <- newServer
  let readerParams = (newServer, backend, sP, params) 
  case sP of
    Writer -> runReaderT writerService readerParams
    Reader -> runReaderT readerService readerParams
    DatabaseServer -> runReaderT databaseService readerParams
    WebServer ->  runReaderT webservice readerParams
    TopicAllocator -> runReaderT topicAllocator readerParams


parseArgs :: IO (ServiceProfile, Text)
parseArgs = do
  [serviceName, lparams] <- getArgs
  let params = pack lparams
  return $ 
    case serviceName of 
      "Writer" -> (Writer, params)
      "Reader" -> (Reader, params)
      "Database" -> (DatabaseServer, params) 
      "Webserver" -> (WebServer, params)
      "TopicAllocator" -> (TopicAllocator, params)
      _  -> throw $ StartUpException $ pack $ serviceName <> "->" <> (unpack params)

remotable ['subscriptionService]

cloudMain :: IO () 
cloudMain = do 
 (sProfile, aPort) <- parseArgs
 backend <- initializeBackend "localhost" (unpack aPort)
                              ( __remoteTable initRemoteTable)
 node <- newLocalNode backend
 Node.runProcess node (subscription backend (sProfile, aPort))

