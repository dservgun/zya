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
    -- * Local caches
        , addConnection
        , deleteConnection
        , broadcastLocalQueues
        , getNextLocalMessage
        , putLocalMessage
        , publishLocalSnapshot
        , messagesTillNow
    -- * server reader
    , ServerReaderT
    -- * Message types.
    , PMessage(..)
    -- ** Some constants.
    , peerTimeout
    -- * Sending and receiving messages
    , sendRemote
    , fireRemote
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
    , MessageT
    , CreateStatus(..)
    -- * Server configuration
    , ServerConfiguration
    , server, backend, serviceProfile
    , serviceName, dbType, connDetails
    , numberOfTestMessages
    , makeServerConfiguration
    , webserverPort
    -- * Publisher details
    , Publisher(..)
    -- * Some common handlers for all nodes
    , handleWhereIsReply
    -- * Manage remote service queues
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
    -- * Common types
    , ClientIdentifier(..)
    -- * Message distribution strategies
    , MessageDistributionStrategy(..)
    , Page(..)
    )
where


import Control.Applicative((<$>))
import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Binary
import Data.List as List
import Data.Map.Strict as Map
import Data.Monoid((<>))
import Data.Text(unpack, Text, pack)
import Data.Time(UTCTime, getCurrentTime)
import Data.Typeable
import Data.Zya.Utils.Logger(debugMessage)
import GHC.Generics (Generic)
import Network.WebSockets.Connection as WS (Connection)
import Text.Printf
import Data.Zya.Core.Internal.MessageDistribution
import Data.Zya.Core.Internal.LocalMessage

-- TODO: Need to deal with this.
type WebServerEndPoint = Int



------------ Constants --------------
peerTimeout :: Int
peerTimeout = 1000000



---------- Basic types  ----


{- | The server unifies remote and local processes to manage logging messages.
    * localClients - For each client identifier, list of topics and their read positions.
    * remoteClients - For each process id the state of the topics.
    * localWriters - Write position for a topic.
    * remoteWriters - Write position for a topic.
    * services - A map of the services running on the network.
    * statistics - A map of requests and responses for the network.
    * proxyChannel - The single channel for inter process communication.
    * myProcessId :
    ** Note: There should exist only one write position per topic.
 -}
data Server = Server {
    localClients :: TVar (Map ClientIdentifier [ClientState])
    , localTBQueue :: TVar (Map ClientIdentifier (WS.Connection, TBQueue LocalMessage))
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

data QueueNotFound =
  QueueNotFound {
      _messageId :: MessageId
    ,_processId :: ProcessId
  } deriving (Show, Typeable)
instance Exception MissingProcessException
instance Exception QueueNotFound


type ServiceName = Text
type Location = Integer
type ErrorCode = Text

newtype Request = Request {unRequest :: Text} deriving Show
newtype Response = Response {unResponse :: Text} deriving Show
newtype ClientIdentifier = ClientIdentifier {unClid :: Text} deriving (Show, Ord, Eq)
data ClientState = ClientState {
    topicCS :: Topic
    , readPos :: Integer
} deriving(Show, Ord, Eq)


type Start = Integer
type End = Integer



newtype Message = Message (UTCTime, Text) deriving (Typeable, Show)
newtype Error =  Error (ErrorCode, Text) deriving (Typeable, Show)
instance Exception Error

newtype StartUpException = StartUpException {_text :: Text} deriving (Typeable, Show)
instance Exception StartUpException


--- Database types
data DBVendor = Postgresql | Sqlite deriving(Show)
data DBType = FileSystem | RDBMS DBVendor deriving(Show)
newtype ConnectionDetails = ConnectionDetails {unStr :: String} deriving (Show)
newtype CreateStatus = CreateStatus {_un :: Text} deriving(Show)
{-| Internal type for persisting process messages -}
type MessageT = ReaderT (DBType, ConnectionDetails, PMessage) IO CreateStatus





data Publisher = Publisher {_unPublish :: Topic} deriving (Show, Typeable, Generic)

data PMessage =
  -- Returns a set of subscribers handled by a process.
  MsgServerInfo Bool ProcessId [Subscriber]
  -- * Notifies a subscriber of the next message.
  | NotifyMessage Subscriber (MessageId, Text)
  -- * Writes a message on a topic.
  | WriteMessage Publisher ProcessId (MessageId, Topic, Text)
  -- * Message Key store information.
  -- UTCTime should probably be replaced with a vector clock.
  | MessageKeyStore (MessageId, ProcessId)
  -- * Commits an offset read for a subscriber.
  | CommitMessage Subscriber (MessageId, Text) -- Commit needs to know about the id that needs to be committed.
  | CommittedWriteMessage Publisher (MessageId, Topic, Text)
  | CommitFailedMessage Publisher (MessageId, Topic, Text)
  -- * Announces that a current service profile is available on a node.
  | ServiceAvailable ServiceProfile ProcessId
  | TerminateProcess Text
  | CreateTopic Text
  -- * When a service becomes available, this message greets the service.
  | GreetingsFrom ServiceProfile ProcessId
  -- Send the message back to the process id
  | QueryMessage (MessageId, ProcessId, Maybe PMessage)
  | ComputeNodeEvent (MessageId, ProcessId, Text)
  deriving (Typeable, Generic, Show)

data FairnessStrategy = RoundRobin | FirstOne deriving (Show)


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
    | ComputeNode
    | Unknown
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
  , _webserverPort :: WebServerEndPoint
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
      <*> newTVarIO Map.empty
      <*> newTChanIO
      <*> newTVarIO m
      <*> newTVarIO Map.empty
      <*> newTVarIO Map.empty
      <*> newTVarIO Map.empty

newServer :: ProcessId -> Process Server
newServer =  liftIO . newServerIO


makeServerConfiguration ::
  Server -> Backend -> ServiceProfile ->
  ServiceName -> DBType -> ConnectionDetails -> Maybe Int ->
  WebServerEndPoint -> ServerConfiguration
makeServerConfiguration s b sp sName db cd aCount anEndpoint =
    ServerConfig s b sp sName db cd aCount anEndpoint
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
  let serviceProfileL = view serviceProfile serverConfiguration
  mynode <- lift getSelfNode

  peers0 <- liftIO $ findPeers backendl peerTimeout

  liftIO $ debugMessage "Data.Zya.Core.TestWriter" ("Initializing process " <> (show serviceNameS) <> "\n")
  let peers = List.filter (/= mynode) peers0
  mypid <- lift getSelfPid
  lift $ register serviceNameS mypid
  forM_ peers $ \peer -> lift $ whereisRemoteAsync peer serviceNameS
  liftIO $ atomically $ do
    _ <- addService server1 serviceProfileL mypid
    updateMyPid server1 mypid


{- | Terminate all processes calling exit on each -}
terminateAllProcesses :: Server -> Process ()
terminateAllProcesses =  \lServer ->  do
  serverConfiguration <- ask
  remoteProcessesL <- liftIO $ atomically $ remoteProcesses lServer
  forM_ remoteProcessesL $ \peer -> exit peer $ TerminateProcess "Shutting down the cloud"


proxyProcess :: Server -> Process ()
proxyProcess aServer
  =  forever $ join $ liftIO $ atomically $ readTChan $ proxyChannel aServer


handleWhereIsReply :: Server -> ServiceProfile -> WhereIsReply -> Process ()
handleWhereIsReply aServer aServiceProfile a@(WhereIsReply _ (Just pid)) = do
--  say $ printf "Handling whereIsReply : "  <> (show serviceProfile) <> " " <> (show a) <> "\n"
  mSpid <-
    liftIO $ do
    currentTime <- getCurrentTime
    atomically $ do
      mySpId <- readTVar $ myProcessId aServer
      sendRemote aServer pid $ (ServiceAvailable aServiceProfile mySpId, currentTime)
      return mySpId
  say $ printf
        ("Sending info about self " <> " " <> (show mSpid) <> ":" <> (show pid) <> (show aServiceProfile)
            <> "\n")
handleWhereIsReply _ _ (WhereIsReply _ Nothing) = return ()




{--|
Update a service queue for round robin or any other strategy.
--}
updateRemoteServiceQueue :: Server -> ProcessId -> (PMessage, UTCTime) -> STM ProcessId
updateRemoteServiceQueue aServer processId (m, time) = do
  serviceEntry <- queryProcessId aServer processId
  case serviceEntry of
    Just (pid, serviceProfileL) -> do
        readTVar (remoteServiceList aServer) >>= \rsl ->
          writeTVar (remoteServiceList aServer) $
              Map.insert (pid, serviceProfileL) time rsl
        return pid
    Nothing ->  return processId --throwSTM $ MissingProcessException processId (pack "Cannot update service queue" )



sendRemote :: Server -> ProcessId -> (PMessage, UTCTime) -> STM ()
sendRemote aServer pid (pmsg, utcTime) = do
    writeTChan (proxyChannel aServer) (send pid pmsg)
    _ <- updateRemoteServiceQueue aServer pid (pmsg, utcTime)
    return ()

getMyPid :: Server -> STM ProcessId
getMyPid = readTVar . myProcessId


updateMyPid :: Server -> ProcessId -> STM ()
updateMyPid aServer processId = flip writeTVar processId $ myProcessId aServer

proxyChannel :: Server -> TChan(Process())
proxyChannel = _proxyChannel

myProcessId :: Server -> TVar ProcessId
myProcessId = _myProcessId

messageKey :: Server -> TVar (Map MessageId ProcessId)
messageKey s = _messageKey s


putLocalMessage :: Server -> ClientIdentifier -> (ProcessId, MessageId) -> STM LocalMessage
putLocalMessage app clientIdentifier (pid, mid) = do
  queueMap <- readTVar $ localTBQueue app
  let result = Map.lookup clientIdentifier queueMap
  case result of
    Nothing -> throwSTM (QueueNotFound mid pid)
    Just (x, queue) -> writeTBQueue queue $ createMessageSummaryP mid pid
  return $ createMessageSummary mid (pack . show $ pid)

getNextLocalMessage :: Server -> ClientIdentifier -> STM [LocalMessage]
getNextLocalMessage app clientIdentifier = do
  queueMap <-readTVar $ localTBQueue app
  let result = Map.lookup clientIdentifier queueMap
  case result of
    Nothing -> return []
    Just (x, queue) -> do
        y <- readTBQueue queue
        return [y]


-- When a service comes up, publish local state
-- This presents a challenge: this snapshot needs to be backed by a
-- persistent globally accessible store.
publishLocalSnapshot :: Server -> ProcessId -> IO ()
publishLocalSnapshot app targetProcessId = do
  liftIO $ putStrLn "Publishing local snapshot"
  messageKeyL <- liftIO $ atomically $ readTVar $ messageKey app
  messageAsList <- return $ Map.assocs messageKeyL
  mapM_ (\(messageId, processId) ->
            liftIO $
              atomically $
                fireRemote app targetProcessId $ MessageKeyStore (messageId, processId)) messageAsList

  return ()





-- Put the messages received till now in the queue.
messagesTillNow :: Server -> ClientIdentifier -> MessageDistributionStrategy -> IO MessageDistributionStrategy
messagesTillNow server clientIdentifier strategy = do
  messageKeyL <- liftIO $ atomically $ readTVar $ messageKey server

  let messageAsList = filterMessages messageKeyL strategy
  mapM_ (\(messageId, processId) ->
            liftIO $
              atomically $ do
                putLocalMessage server clientIdentifier (processId, messageId)) messageAsList
  return strategy
  where
    filterMessages aMap strategy =
        case strategy of
          Begin -> Map.assocs aMap
          End -> Map.assocs $ Map.empty
          Last (n, (Page p)) -> List.take (n * p) $ Map.assocs aMap


-- IO because we want to break up each individual sends, or have
-- some control on how many message sends should we buffer.
broadcastLocalQueues :: Server -> (ProcessId, MessageId) -> IO ()
broadcastLocalQueues server (processIdL, messageIdL) = do
  localQueues <- atomically $ readTVar $ localTBQueue server
  let elems = Map.elems localQueues
  putStrLn $ "Publishing to Local queues " <> (show processIdL) <> ", " <> (show messageIdL)
  mapM_ (\(conn, queue) -> atomically $ writeTBQueue queue
        (createMessageSummary messageIdL (pack $ show processIdL))) elems
  return ()

-- Query a process id for its service profile
queryProcessId :: Server -> ProcessId -> STM(Maybe (ProcessId, ServiceProfile))
queryProcessId  =
  \lServer pid -> do
    servicesL <- readTVar $ services lServer
    let result = keys $ Map.filterWithKey(\(procId, _) _ -> procId == pid) servicesL
    case result of
      h : t -> return . Just $ h
      _ -> return Nothing


{- |
  Given a service profile, return the count of services and the ProcessId for the service.
-}
queryService :: Server -> ServiceProfile -> STM[(ProcessId, ServiceProfile, Integer)]
queryService aServer aProfile = do
  servicesL <- readTVar $ services aServer
  let result = List.filter (\((_, profile), _) -> profile == aProfile) $ Map.assocs servicesL
  let r = List.map (\((x, y), z) -> (x, y, z)) result
  return r

{- |
  Merge all the remote processes collecting ` remoteClients `
  `remoteWriters` and `services`. The remote client list
  ought to be a superset of services and writers.
-}

remoteProcesses :: Server -> STM[ProcessId]
remoteProcesses aServer = do
  myProcessIdL <- readTVar $ myProcessId aServer
  remoteClientsL <- readTVar $ remoteClients aServer
  remoteWritersL <- readTVar $ remoteWriters aServer
  remoteServicesL <- readTVar $ services aServer
  serviceMap <- readTVar $ services aServer
  let result = List.map fst $ Map.keys remoteClientsL
  let r2 = List.map fst $ Map.keys remoteWritersL
  let r3 = List.map fst $ Map.keys serviceMap
  return $ List.filter (/= myProcessIdL) $ result <> r2 <> r3

type ServiceRange = (Int, Int)

{- | A typical default configuration.
-}
defaultSimpleConfiguration :: [(ServiceProfile, ServiceRange)]
defaultSimpleConfiguration = [(WebServer, (3, 10)), (QueryService, (3, 10)),
                        (Reader, (3, 10)),
                        (Writer, (3, 10))]


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
findAvailableWriter = \serverL -> findAvailableService serverL Writer RoundRobin


queryFallbackservice :: Server -> ServiceProfile -> STM (Maybe ProcessId)
queryFallbackservice = \serverL serviceProfileL ->
  do
    servicesL <- readTVar $ services serverL
    let entries =
          Map.keys $
            Map.filterWithKey(\(_, sProfile) _ -> sProfile == serviceProfileL) servicesL

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
findAvailableService aServer sP RoundRobin = do
  servicesL <- readTVar $ remoteServiceList aServer
  let spl = List.map(\((x, y), _) -> x) $
              List.sortBy(\(_, time1) (_, time2) -> time1 `compare` time2) $
              List.filter (\((pid, serviceProfileL), time) -> serviceProfileL == sP)
                $ Map.toList servicesL
  case spl of
    [] -> queryFallbackservice aServer sP
    h : _ -> return . Just $ h

findAvailableService aServer sP FirstOne = queryFallbackservice aServer sP



-- TODO: clean this up.
removeProcess :: Server -> ProcessId -> STM ProcessId
removeProcess aServer processId = do
  remoteClientsL <- readTVar $ remoteClients aServer
  writeTVar (remoteClients aServer) $
    Map.filterWithKey(\(prId, _) _ -> prId /= processId) remoteClientsL

  servicesL <- readTVar $ services aServer
  writeTVar (services aServer) $ Map.filterWithKey(\(prId, _) _ -> processId /= prId) servicesL

  remWriters <- readTVar $ remoteWriters aServer
  writeTVar (remoteWriters aServer) $
    Map.filterWithKey(\(prId, _) _ -> processId /= prId) remWriters

  remoteServiceQueue <- readTVar $ remoteServiceList aServer
  writeTVar (remoteServiceList aServer)
    $ Map.filterWithKey(\(prId, _) _ -> prId /= processId) remoteServiceQueue
  return processId

{-- |
  Add 'ServiceProfile' to the local map
--}
addService :: Server -> ServiceProfile-> ProcessId -> STM ProcessId
addService aServer aServiceProfile processId = do
  s1 <- readTVar $ services aServer
  writeTVar (services aServer)
    $ Map.insertWith (+) (processId, aServiceProfile) 1 s1
  return processId

-- Update the messageId with a processId.
updateMessageKey :: Server -> ProcessId -> MessageId -> STM ProcessId
updateMessageKey aServer processId messageId = do
          messageKeyL <- readTVar (messageKey aServer)
          writeTVar (messageKey aServer)
            $ Map.insert messageId processId messageKeyL
          return processId

{--| This should give the approximate count of the number of messages processed by the cloud.
   | Since the message key is a map, this value will ignore duplicates that may have been processed.
--}
queryMessageKeyCount :: Server -> STM Int
queryMessageKeyCount = \serverL -> readTVar (messageKey serverL) >>= return . Map.size

{-- | Query the local message counts. This count is different from the actual messages processed as far as
    | the current process is concerned. Use ** queryMessageKeyCount ** to get an estimate of how many messages
    | got processed by the cloud.
--}
queryMessageCount :: Server -> STM Int
queryMessageCount serverL = readTVar (_messageValues serverL) >>= return . Map.size

updateMessageValue :: Server -> MessageId -> PMessage -> STM PMessage
updateMessageValue serverL messageId aMessage = do
  readTVar (_messageValues serverL) >>= \x ->
    writeTVar (_messageValues serverL) $
      Map.insert messageId aMessage x
  return aMessage


{-- | Update the connection information with queue to merge events.
--}
addConnection :: Server -> ClientIdentifier ->
                    WS.Connection -> STM (ClientIdentifier, WS.Connection, TBQueue LocalMessage)
addConnection serverL clientIdentifier aConnection = do
  readTVar (localTBQueue serverL) >>= \x -> do
    -- TODO : Read the queue bounds from a reader.
    queue <- newTBQueue 100 -- change this to a better number after testing.
    writeTVar (localTBQueue serverL) $
      Map.insert clientIdentifier (aConnection, queue) x
    return (clientIdentifier, aConnection, queue)
deleteConnection :: Server -> ClientIdentifier -> STM (Maybe(Connection))
deleteConnection serverL aClientIdentifier = do
  x <- readTVar (localTBQueue serverL)
  let connPair = Map.lookup aClientIdentifier x
  writeTVar (localTBQueue serverL) $
    Map.delete aClientIdentifier x
  return $ fmap fst connPair


queryMessageValue :: Server -> MessageId -> STM (Maybe PMessage)
queryMessageValue aServer messageKey1 =
  readTVar (_messageValues aServer) >>= \x -> return $ Map.lookup messageKey1 x

queryMessageLocation :: Server -> MessageId -> STM (Maybe ProcessId)
queryMessageLocation aServer messageId =
    readTVar (_messageLocation aServer)
      >>= \ x -> return (Map.lookup messageId x)

-- Update message query location
updateMessageLocation :: Server -> ProcessId -> MessageId -> STM ProcessId
updateMessageLocation aServer processId messageId =
    readTVar (_messageLocation aServer)
        >>= \x -> do
          writeTVar (_messageLocation aServer) $
            Map.insert messageId processId x
          return processId

{--|
  Use this to send messages that are most likely one time sends, such as service
  available, greetings etc. These dont need to be timestamped.
--}
fireRemote :: Server -> ProcessId -> PMessage -> STM ()
fireRemote aServer pid pmsg = do
  writeTChan (proxyChannel aServer) (send pid pmsg)
  return ()

-- Publish the key info to all the services other than self.
publishMessageKey :: Server -> ProcessId -> MessageId -> STM ProcessId
publishMessageKey aServer processId messageId = do
    rProcesses <- remoteProcesses aServer
    mapM_ (\pid -> fireRemote aServer pid (MessageKeyStore (messageId, processId)))
      rProcesses
    return processId


startupException :: Text -> StartUpException
startupException = StartUpException



instance Binary PMessage
instance Binary ServiceProfile
instance Binary Publisher
