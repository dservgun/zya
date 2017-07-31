{-# LANGUAGE DeriveGeneric #-}
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
        , updateTopicAllocator
        , updateMyPid
        , remoteProcesses
        , defaultSimpleConfiguration
        , isSingleton
        , findAvailableWriter
        , getMyPid
    )
where 
import Data.Monoid((<>))
import Data.Map as Map
import Data.List as List
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Distributed.Process
import Data.Text 
import Text.Printf
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable

newtype Request = Request {unRequest :: Text} deriving Show 
newtype Response = Response {unResponse :: Text} deriving Show
newtype ClientIdentifier = ClientIdentifier {unClid :: Text} deriving (Show)
newtype Topic = Topic {unTopic :: Text} deriving Show
data ClientState = ClientState {
    topic :: Topic
    , readPos :: Integer
}

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
    ,  services :: TVar (Map (ProcessId, ServiceProfile) Integer)
    , statistics :: TVar (Map ProcessId ([Request], [Response]))
    , _proxyChannel :: TChan(Process())
    , _myProcessId :: TVar (Maybe ProcessId)
}

-- Accessors. TODO: Use lenses. At times, possessives in method names
-- works.
getMyPid :: Server -> STM (Maybe ProcessId)
getMyPid server = readTVar $ myProcessId server

updateMyPid :: Server -> ProcessId -> STM () 
updateMyPid server processId = writeTVar (myProcessId server) (Just processId)

proxyChannel :: Server -> TChan(Process()) 
proxyChannel = _proxyChannel

myProcessId :: Server -> TVar (Maybe ProcessId) 
myProcessId = _myProcessId

newServerIO :: IO Server 
newServerIO = do
    localClients <- newTVarIO Map.empty
    remoteClientMap <- newTVarIO Map.empty
    localWriterMap <- newTVarIO Map.empty 
    remoteWriterMap <- newTVarIO Map.empty
    serviceMap <- newTVarIO Map.empty
    statistics <- newTVarIO Map.empty
    proxyChannel <- newTChanIO
    initProcessId <- newTVarIO Nothing
    return Server {
        localClients = localClients
        , remoteClients = remoteClientMap 
        , localWriters = localWriterMap 
        , remoteWriters = remoteWriterMap
        , services = serviceMap 
        , statistics = statistics
        , _proxyChannel = proxyChannel
        , _myProcessId = initProcessId 
    }
newServer :: Process Server 
newServer =  
    liftIO newServerIO





{- | Update the service map with the topic allocator. We really need a reliable 
   | consensus to deal with this class of problems.
-}
updateTopicAllocator :: Server -> Maybe ProcessId -> ServiceProfile -> STM () 
updateTopicAllocator server (Just processId) TopicAllocator = do 
  lServices <- readTVar $ services server 
  let nElement = ((processId, TopicAllocator), 1)
  let updateServices = uncurry Map.insert nElement lServices
  -- Clever code alert: though, using flip takes away the need to 
  -- parenthesize.
  flip writeTVar updateServices $ services server
updateTopicAllocator server _ _ = return () -- Error, we need to tell it somewhere.
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
remoteProcesses :: Server -> STM [ProcessId] 
remoteProcesses server = do 
  myProcessId <- readTVar $ myProcessId server
  case myProcessId of
      Nothing -> return []
      Just pyd -> do 
          remoteClients <- readTVar $ remoteClients server
          remoteWriters <- readTVar $ remoteWriters server 
          remoteServices <- readTVar $ services server
          serviceMap <- readTVar $ services server
          let result = List.map fst $ Map.keys remoteClients 
          let r2 = List.map fst $ Map.keys remoteWriters 
          let r3 = List.map fst $ Map.keys serviceMap
          return $ List.filter (/= pyd) $ result <> r2 <> r3

type ServiceRange = (Int, Int) 

{- | A typical default configuration.
-}
defaultSimpleConfiguration :: [(ServiceProfile, ServiceRange)]
defaultSimpleConfiguration = [(WebServer, (3, 10)), (DatabaseServer, (3, 10)), 
                        (Reader, (3, 10)), 
                        (Writer, (3, 10))]

{- | Supported services -}
data ServiceProfile = 
    WebServer 
    | DatabaseServer 
    | Reader 
    | Writer 
    | TopicAllocator
    | Terminator
    deriving(Show, Generic, Typeable, Eq, Ord)

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
findAvailableWriter server = do 
  writers <- readTVar $ remoteClients server 
  let keys = Map.keys writers 
  return $ 
    case keys of
      h : t -> Just $ fst h
      _ ->  Nothing



