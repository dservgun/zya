{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.Service
    (
        -- * The supported service profiles
        ServiceProfile(..)
        -- New server
        , newServer
        , newServerIO
        , Server
        , proxyChannel
        , myProcessId
        , updateMyPid
        , remoteProcesses
        , defaultSimpleConfiguration
        , isSingleton
        , findAvailableWriter
        , findAvailableService
        , getMyPid
        -- * Update maps 
        , removeProcess
        -- * Maintain services 
        , addService
        , queryProcessId
        , remoteServiceList
        , publishMessageKey
        , terminateAllProcesses
    -- * server reader 
    , ServerReaderT
    -- * Message types.
    , PMessage(..)
    -- ** Some constants.
    , peerTimeout
    -- * Sending and receiving messages
    , sendRemote
    -- * Initializing the cloud process
    , initializeProcess
    , subscriptionService
    , ServiceName
    -- ** Exceptions and constructors
    , StartUpException
    , startupException
    -- ** Some utility functions
    , proxyProcess
    -- * Database constants
    , DBType(..)
    , ConnectionDetails(..)
    , DBVendor(..)
    , MessageT(..)
    , CreateStatus(..)
    -- * Server configuration 
    , ServerConfiguration
    , server, backend, serviceProfile  
    , serviceName, dbType, connDetails
    , numberOfTestMessages
    , makeServerConfiguration
    -- * Publisher details 
    , Publisher(..)
    -- * Some common handlers for all nodes
    , handleWhereIsReply
    -- * Manage remote service queues
    , updateRemoteServiceQueue
    , updateMessageKey
    , updateMessageLocation
    , updateMessageValue
    , queryMessageValue
    , queryMessageLocation
    -- * Approximate count of the total messages processed by the cloud.
    , queryMessageKeyCount 
    -- * Total messages handled till now locally.
    , queryMessageCount
    , Topic(..)
    , FairnessStrategy(..)
    )
where 
import Data.Monoid((<>))
import Data.Map as Map
import Data.Set as Set
import Data.List as List
import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Distributed.Process
import Data.Time
import Data.Text 
import Text.Printf
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import System.Environment(getArgs)

import Control.Concurrent.STM
import Control.Applicative((<$>))
import Control.Exception

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Reader

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)
import Control.Lens

import Data.Binary
import Data.Data
import Data.Monoid((<>))
import Data.Text(pack, unpack, take, Text)
import Data.Time(UTCTime, getCurrentTime)
import Data.Typeable
import Text.Printf



------------ Constants --------------
peerTimeout :: Int
peerTimeout = 1000000



--MAX_BYTES :: Integer 
maxBytes = 10 * 1024 * 1024 * 1024 -- 

trim :: Int -> Text -> Text 
trim = Data.Text.take




---------- Basic types  ----


{- | The server unifies remote and local processes to manage logging messages.
    * localClients - For each client identifier, list of topics and their read positions.
    * remoteClients - For each process id the state of the topics.
    * localWriters - Write position for a topic.
    * remoteWriters - Write position for a topic. 
    * services - A map of the services running on the network.
    ** Note: There should exist only one write position per topic.
 -}
data Server = Server {
    localClients :: TVar (Map ClientIdentifier [ClientState])
    , remoteClients :: TVar (Map (ProcessId, ClientIdentifier) [ClientState])
    , localWriters :: TVar (Map Topic Integer)
    , remoteWriters :: TVar (Map (ProcessId, Topic) Integer)
    , remoteServiceList :: TVar (Map (ProcessId, ServiceProfile) UTCTime)
    ,  services :: TVar (Map (ProcessId, ServiceProfile) Integer)
    , statistics :: TVar (Map ProcessId ([Request], [Response]))
    , _proxyChannel :: TChan(Process())
    , _myProcessId :: TVar (ProcessId)
    , _messageKey :: TVar (Map MessageId ProcessId)
    -- The location of the message in the cluster of query services
    , _messageLocation :: TVar(Map MessageId ProcessId)
    -- Except for query services, the rest of the processes wont be populating 
    -- this cache. This should be modeled as a service level cache,
    -- that each kind of service should handle.
    , _messageValues :: TVar(Map MessageId PMessage)
}


{--| 
  An exception condition when process id was expected to be present.
--}
data MissingProcessException =  
              MissingProcessException
                {_unProcess :: ProcessId
                , message :: Text} deriving (Show, Typeable)
instance Exception MissingProcessException


type PageSize = Integer

type ServiceName = Text
type Location = Integer
type ErrorCode = Text

newtype Request = Request {unRequest :: Text} deriving Show 
newtype Response = Response {unResponse :: Text} deriving Show
newtype ClientIdentifier = ClientIdentifier {unClid :: Text} deriving (Show, Ord, Eq)
newtype Topic = Topic {unTopic :: Text} deriving (Show, Ord, Eq, Generic)
data ClientState = ClientState {
    topicCS :: Topic
    , readPos :: Integer
} deriving(Show, Ord, Eq)


type Start = Integer
type End = Integer 


-- The message id is unique among all the processes.
type MessageId = Text
data OffsetHint = Beginning | Latest | MessageRange (Start , End) deriving (Show, Typeable, Generic)

newtype Message = Message (UTCTime, Text) deriving (Typeable, Show)
newtype Error =  Error (ErrorCode, Text) deriving (Typeable, Show) 
instance Exception Error

newtype StartUpException = StartUpException Text deriving (Typeable, Show)
instance Exception StartUpException


--- Database types
data DBVendor = Postgresql | Sqlite
data DBType = FileSystem | RDBMS DBVendor 
newtype ConnectionDetails = ConnectionDetails {unStr :: String} deriving (Show)
newtype CreateStatus = CreateStatus {_un :: Text} deriving(Show)
{-| Internal type for persisting process messages -}
type MessageT = ReaderT (DBType, ConnectionDetails, PMessage) IO CreateStatus

--------------Application types ---
type CommitOffset = Integer 
data User = User {
  login :: Login
  , topics :: [(Topic, CommitOffset)]
} deriving (Show, Typeable, Generic)
data OpenIdProvider = Google | Facebook | LinkedIn deriving (Show, Typeable, Generic)
{- | Email needs to be validated. TODO
-}
type Email = Text 
{- | Support for login based on the email id and the open id. 
-}
data Login = Login {
    email :: Email 
    , openId :: OpenIdProvider
} deriving (Show, Typeable, Generic)



data Subscriber = 
  Subscriber {
    topic :: Topic
    , user :: User
    , reader :: OffsetHint
} deriving (Show, Typeable, Generic)

data Publisher = Publisher {_unPublish :: Topic} deriving (Show, Typeable, Generic)

data PMessage = 
  -- Returns a set of subscribers handled by a process.
  MsgServerInfo Bool ProcessId [Subscriber]
  -- * Notifies a subscriber of the next message.
  | NotifyMessage Subscriber (MessageId, Text)
  -- * Writes a message on a topic. 
  | WriteMessage Publisher (MessageId, Topic, Text)
  -- * Message Key store information.
  -- UTCTime should probably be replaced with a vector clock.
  | MessageKeyStore (MessageId, ProcessId)
  -- * Commits an offset read for a subscriber.
  | CommitMessage Subscriber (MessageId, Text) -- Commit needs to know about the id that needs to be committed.
  | CommittedWriteMessage Publisher (MessageId, Topic, Text)
  -- * Announces that a current service profile is available on a node.
  | ServiceAvailable ServiceProfile ProcessId 
  | TerminateProcess Text
  | CreateTopic Text 
  -- * When a service becomes available, this message greets the service.
  | GreetingsFrom ServiceProfile ProcessId  
  -- Send the message back to the process id
  | QueryMessage (MessageId, ProcessId, Maybe PMessage)
  deriving (Typeable, Generic)

data FairnessStrategy = RoundRobin | FirstOne deriving(Show)


{- | Supported services -}
data ServiceProfile = 
    WebServer 
    | Reader 
    | Writer 
    | QueryService
    | TopicAllocator
    -- * Terminate all processes. This may not be needed.
    | Terminator
    -- * A writer to test some messages to the system.
    | TestWriter 
    deriving(Show, Generic, Typeable, Eq, Ord)


type ServerReaderT = ReaderT ServerConfiguration Process

data ServerConfiguration = ServerConfig{
   _server :: Server 
  , _backend :: Backend 
  , _serviceProfile :: ServiceProfile 
  , _serviceName :: ServiceName 
  , _dbType :: DBType 
  , _connDetails :: ConnectionDetails
  , _numberOfTestMessages :: Maybe Int
  } 


makeLenses ''ServerConfiguration
{--| 
  * Initialization.
--}
newServerIO :: ProcessId -> IO Server 
newServerIO =
  \m -> 
    Server 
      <$> newTVarIO Map.empty
      <*> newTVarIO Map.empty
      <*> newTVarIO Map.empty
      <*> newTVarIO Map.empty
      <*> newTVarIO Map.empty 
      <*> newTVarIO Map.empty
      <*> newTVarIO Map.empty
      <*> newTChanIO 
      <*> newTVarIO m
      <*> newTVarIO Map.empty
      <*> newTVarIO Map.empty
      <*> newTVarIO Map.empty

newServer :: ProcessId -> Process Server 
newServer =  liftIO . newServerIO



makeServerConfiguration :: 
  Server -> Backend -> ServiceProfile -> ServiceName -> DBType -> ConnectionDetails -> Maybe Int -> ServerConfiguration
makeServerConfiguration s b sp sName db cd aCount = ServerConfig s b sp sName db cd aCount 
subscriptionService :: String -> Process () 
subscriptionService aPort = return ()


{--| 
  Service initialization and generic handlers.
--}
initializeProcess :: ServerReaderT()
initializeProcess = do 
  serverConfiguration <- ask
  let server1 = view server serverConfiguration
  let serviceName1 = view serviceName serverConfiguration
  let serviceNameS = unpack serviceName1
  let backendl = view backend serverConfiguration
  mynode <- lift getSelfNode

  peers0 <- liftIO $ findPeers backendl peerTimeout
  let peers = List.filter (/= mynode) peers0
  mypid <- lift getSelfPid
  lift $ register serviceNameS mypid
  forM_ peers $ \peer -> lift $ whereisRemoteAsync peer serviceNameS
  liftIO $ atomically $ do 
    updateMyPid server1 mypid


{- | Terminate all processes calling exit on each -}
terminateAllProcesses :: Server -> Process ()
terminateAllProcesses server = do 
  serverConfiguration <- ask
  remoteProcesses <- liftIO $ atomically $ remoteProcesses server
  forM_ remoteProcesses $ \peer -> exit peer $ TerminateProcess "Shutting down the cloud"
  pid <- getSelfPid -- the state is not updated in the terminator, at least for now.
  exit pid $ TerminateProcess "Shutting down self"


proxyProcess :: Server -> Process ()
proxyProcess server 
  =  forever $ join $ liftIO $ atomically $ readTChan $ proxyChannel server


-- Announce that a service has come up. 
-- When a service receives this message, it needs to send some info 
-- about itself to the new service. Will this result in n squared messages.

handleWhereIsReply :: Server -> ServiceProfile -> WhereIsReply -> Process ()
handleWhereIsReply server serviceProfile a@(WhereIsReply _ (Just pid)) = do
--  say $ printf "Handling whereIsReply : "  <> (show serviceProfile) <> " " <> (show a) <> "\n"
  mSpid <- 
    liftIO $ do
    currentTime <- getCurrentTime
    atomically $ do
      mySpId <- readTVar $ myProcessId server
      sendRemote server pid $ (ServiceAvailable serviceProfile mySpId, currentTime)
      return mySpId
  say $ printf 
        ("Sending info about self " <> " " <> (show mSpid) <> ":" <> (show pid) <> (show serviceProfile) 
            <> "\n")
handleWhereIsReply _ serviceProfile (WhereIsReply _ Nothing) = return ()



{--| 
Update a service queue for round robin or any other strategy.
--}
updateRemoteServiceQueue :: Server -> ProcessId -> (PMessage, UTCTime) -> STM ProcessId
updateRemoteServiceQueue server processId (m, time) = do 
  (procId, servProfile) <- queryProcessId server processId 
  case servProfile of 
    Just sP -> do 
        readTVar (remoteServiceList server) >>= \rsl -> 
          writeTVar (remoteServiceList server) $ 
              Map.insert (procId, sP) time rsl
        return procId
    Nothing ->  return processId --throwSTM $ MissingProcessException processId (pack "Cannot update service queue" )



sendRemote :: Server -> ProcessId -> (PMessage, UTCTime) -> STM ()
sendRemote aServer pid (pmsg, utcTime) = do 
    writeTChan (proxyChannel aServer) (send pid pmsg)
    _ <- updateRemoteServiceQueue aServer pid (pmsg, utcTime)
    return ()


-- Accessors. TODO: Use lenses. At times, possessives in method names
-- works.
getMyPid :: Server -> STM ProcessId
getMyPid server = readTVar $ myProcessId server


updateMyPid :: Server -> ProcessId -> STM () 
updateMyPid server processId = writeTVar (myProcessId server) processId

proxyChannel :: Server -> TChan(Process()) 
proxyChannel = _proxyChannel

myProcessId :: Server -> TVar ProcessId
myProcessId = _myProcessId

messageKey :: Server -> TVar (Map MessageId ProcessId) 
messageKey s = _messageKey s 





-- Query a process id for its service profile
queryProcessId :: Server -> ProcessId -> STM(ProcessId, Maybe ServiceProfile)
queryProcessId server = 
  \pid -> do
    services <- readTVar $ services server 
    let result = keys $ Map.filterWithKey(\(procId, _) _ -> procId == pid) services
    case result of
      [h] -> return (pid, Just . snd $ h)
      _ -> return(pid, Nothing)


{- | 
  Given a service profile, return the count of services and the ProcessId for the service.
-}
queryService :: Server -> ServiceProfile -> STM[(ProcessId, ServiceProfile, Integer)]
queryService server aProfile = do 
  services <- readTVar $ services server 
  let result = List.filter (\((x, y), z) -> y == aProfile) $ Map.assocs services
  let r = List.map (\((x, y), z) -> (x, y, z)) result
  return r

{- | 
  Merge all the remote processes collecting ` remoteClients `
  `remoteWriters` and `services`. The remote client list 
  ought to be a superset of services and writers. 
-}

remoteProcesses :: Server -> STM[ProcessId]
remoteProcesses server = do 
  myProcessId <- readTVar $ myProcessId server
  remoteClients <- readTVar $ remoteClients server
  remoteWriters <- readTVar $ remoteWriters server 
  remoteServices <- readTVar $ services server
  serviceMap <- readTVar $ services server
  let result = List.map fst $ Map.keys remoteClients 
  let r2 = List.map fst $ Map.keys remoteWriters 
  let r3 = List.map fst $ Map.keys serviceMap
  return $ List.filter (/= myProcessId) $ result <> r2 <> r3

type ServiceRange = (Int, Int) 

{- | A typical default configuration.
-}
defaultSimpleConfiguration :: [(ServiceProfile, ServiceRange)]
defaultSimpleConfiguration = [(WebServer, (3, 10)), (QueryService, (3, 10)), 
                        (Reader, (3, 10)), 
                        (Writer, (3, 10))]


instance Binary ServiceProfile

{-| 
  A global map indicating if a particular service is a singleton, ideally a leader should fix the 
  need for this map. 
-}
isSingleton :: ServiceProfile -> Bool
isSingleton TopicAllocator = True
isSingleton _ = False

{- | Find an available writer or return None. Find the first writer
  , though find the one with the least number of topics or messages or both.
-}
findAvailableWriter :: Server -> STM (Maybe ProcessId)
findAvailableWriter server = findAvailableService server Writer RoundRobin


queryFallbackservice :: Server -> ServiceProfile -> STM (Maybe ProcessId)
queryFallbackservice server serviceProfile = do 
  services <- readTVar $ services server 
  let entries = 
        Map.keys $ 
          Map.filterWithKey(\(_, sProfile) _ -> sProfile == serviceProfile) services

  res <-
      case entries of
        h : t -> do 
          return . Just $ fst h
        _ ->  return Nothing
  
  return res



{--| 
  Find a service to write to. Use a simple strategy
--}

findAvailableService :: Server -> ServiceProfile -> FairnessStrategy -> STM(Maybe ProcessId) 
findAvailableService server sP RoundRobin = do 
  services <- readTVar $ remoteServiceList server  
  let spl = List.map(\((x, y), _) -> x) $
              List.sortBy(\(_, time1) (_, time2) -> time1 `compare` time2) $ 
              List.filter (\((pid, serviceProfile), time) -> serviceProfile == sP) 
                $ Map.toList services
  case spl of 
    [] -> queryFallbackservice server sP
    h : _ -> return . Just $ h  

findAvailableService server sP FirstOne = queryFallbackservice server sP




removeProcess :: Server -> ProcessId -> STM ProcessId 
removeProcess server processId = do 
  remoteClients1 <- readTVar $ remoteClients server 
  writeTVar (remoteClients server) $
    Map.filterWithKey(\(prId, _) c -> prId /= processId) remoteClients1

  services1 <- readTVar $ services server 
  writeTVar (services server) $ Map.filterWithKey(\(prId, _) _ -> processId /= prId) services1

  remWriters <- readTVar $ remoteWriters server 
  writeTVar (remoteWriters server) $
    Map.filterWithKey(\(prId, _) _ -> processId /= prId) remWriters

  remoteServiceQueue <- readTVar $ remoteServiceList server 
  writeTVar (remoteServiceList server) 
    $ Map.filterWithKey(\(prId, _) _ -> prId /= processId) remoteServiceQueue
  return processId

{-- | 
  Add 'ServiceProfile' to the local map
--}
addService :: Server -> ServiceProfile-> ProcessId -> STM ProcessId
addService server serviceProfile processId = do 
  s1 <- readTVar $ services server
  writeTVar (services server) 
    $ Map.insertWith (+) (processId, serviceProfile) 1 s1
  return processId

-- Update the messageId with a processId.
updateMessageKey :: Server -> ProcessId -> MessageId -> STM ProcessId 
updateMessageKey server processId messageId = do 
          messageKeyL <- readTVar (messageKey server)
          writeTVar (messageKey server)
            $ Map.insert messageId processId messageKeyL
          return processId

{--| This should give the approximate count of the number of messages processed by the cloud.
   | Since the message key is a map, this value will ignore duplicates that may have been processed.

--}
queryMessageKeyCount :: Server -> STM Int 
queryMessageKeyCount server = readTVar (messageKey server) >>= return . Map.size
{-- | Query the local message counts. This count is different from the actual messages processed as far as 
    | the current process is concerned. Use ** queryMessageKeyCount ** to get an estimate of how many messages
    | got processed by the cloud.
--}
queryMessageCount :: Server -> STM Int 
queryMessageCount server = readTVar (_messageValues server) >>= return . Map.size

updateMessageValue :: Server -> MessageId -> PMessage -> STM PMessage 
updateMessageValue server messageKey aMessage = do 
  readTVar (_messageValues server) >>= \x -> 
    writeTVar (_messageValues server) $ 
      Map.insert messageKey aMessage x
  return aMessage

queryMessageValue :: Server -> MessageId -> STM (Maybe PMessage) 
queryMessageValue server messageKey = 
  readTVar (_messageValues server) >>= \x -> return $ Map.lookup messageKey x

queryMessageLocation :: Server -> MessageId -> STM (Maybe ProcessId)
queryMessageLocation server messageId = 
    readTVar (_messageLocation server)
      >>= \ x -> return (Map.lookup messageId x)
-- Update message query location 
updateMessageLocation :: Server -> ProcessId -> MessageId -> STM ProcessId 
updateMessageLocation server processId messageId =
    readTVar (_messageLocation server)
        >>= \x -> do 
          writeTVar (_messageLocation server) $ 
            Map.insert messageId processId x 
          return processId
fireRemote :: Server -> ProcessId -> PMessage -> STM ()
fireRemote aServer pid pmsg = do 
  writeTChan (proxyChannel aServer) (send pid pmsg)
  return ()

-- Publish the key info to all the services other than self.
publishMessageKey :: Server -> ProcessId -> MessageId -> STM ProcessId 
publishMessageKey server processId messageId = do 
    rProcesses <- remoteProcesses server
    mapM_ (\pid -> fireRemote server pid (MessageKeyStore (messageId, processId)))
      rProcesses
    return processId


startupException :: Text -> StartUpException
startupException = StartUpException


instance Show PMessage where 
  show pMessage = 
      case pMessage of 
        MsgServerInfo a b l -> printf "MsgServerInfo " <>(show a) <> ":"  <> show b <> " " <> (show l) <> "\n"
        NotifyMessage s (m, t) -> 
            printf "Notify message " 
              <> (show s) <> ":" <> (show m) <> (unpack $ trim maxBytes t) <> "\n"
        WriteMessage p (m, topic, t) -> 
          printf "WriteMessage " <> (show p) <> ":" <> (show m) <> ":" <> (show topic) <> (unpack $ trim maxBytes t) <> "\n"
        CommitMessage s (m, t) -> printf "Commit message " <> (show s) <> (show m) <> (unpack $ trim maxBytes t) <> "\n"
        ServiceAvailable s p -> printf "ServiceAvailable " <>(show s) <> ":" <> (show p)  <> "\n"
        TerminateProcess s  -> printf "TerminateProcess " <> (show s) <> "\n"
        CreateTopic t -> printf "CreateTopic " <> (show . unpack $ trim maxBytes t) <> "\n"
        GreetingsFrom s p -> printf "Greetings from " <> (show s) <> " " <> (show p) <> "\n"
        CommittedWriteMessage p (m, topic, t) -> 
          printf "CommittedWriteMessage " <> (show p) <> ":" <> (show m) <> ":" <> (show topic) <> (unpack $ trim maxBytes t) <> "\n"

instance Binary Login 
instance Binary OpenIdProvider
instance Binary User
instance Binary OffsetHint
instance Binary Subscriber
instance Binary Publisher
instance Binary PMessage
instance Binary Topic
