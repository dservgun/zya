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
        , updateMyPid
        , remoteProcesses
        , defaultSimpleConfiguration
        , isSingleton
        , findAvailableWriter
        , getMyPid
        -- * Update maps 
        , removeProcess
        -- * Maintain services 
        , addService
        , queryProcessId
        , remoteServiceList
    )
where 
import Data.Monoid((<>))
import Data.Map as Map
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

newtype Request = Request {unRequest :: Text} deriving Show 
newtype Response = Response {unResponse :: Text} deriving Show
newtype ClientIdentifier = ClientIdentifier {unClid :: Text} deriving (Show, Ord, Eq)
newtype Topic = Topic {unTopic :: Text} deriving (Show, Ord, Eq)
data ClientState = ClientState {
    topic :: Topic
    , readPos :: Integer
} deriving(Show, Ord, Eq)



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
    , remoteServiceList :: TVar [(ProcessId, ServiceProfile, UTCTime)]
    ,  services :: TVar (Map (ProcessId, ServiceProfile) Integer)
    , statistics :: TVar (Map ProcessId ([Request], [Response]))
    , _proxyChannel :: TChan(Process())
    , _myProcessId :: TVar (ProcessId)
}

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


newServerIO :: ProcessId -> IO Server 
newServerIO =
  \m -> 
    Server 
      <$> newTVarIO Map.empty
      <*> newTVarIO Map.empty
      <*> newTVarIO Map.empty
      <*> newTVarIO Map.empty
      <*> newTVarIO [] 
      <*> newTVarIO Map.empty
      <*> newTVarIO Map.empty
      <*> newTChanIO 
      <*> newTVarIO m


newServer :: ProcessId -> Process Server 
newServer =  liftIO . newServerIO



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
    -- * Terminate all processes. This may not be needed.
    | Terminator
    -- * A writer to test some messages to the system.
    | TestWriter 
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
  writers <- readTVar $ services server 
  let entries = 
        Map.keys $ 
          Map.filterWithKey(\(_, sProfile) _ -> sProfile == Writer) writers

  res <-
      case entries of
        h : t -> do 
          return . Just $ fst h
        _ ->  return Nothing
  
  return res

data FairnessStrategy = RoundRobin | FirstOne deriving(Show)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (h:_) = Just h

queryAnyAvailableService :: Server -> ServiceProfile -> STM (Maybe ProcessId)
queryAnyAvailableService server serviceProfile = do 
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
  Find a service to write to. Use a simple round robin strategy.
--}

findAvailableService :: Server -> ServiceProfile -> FairnessStrategy -> STM(Maybe ProcessId) 
findAvailableService server sP RoundRobin = do 
  rsl <- readTVar $ remoteServiceList server
  let spl = List.map(\(x, y, _) -> x) $
              List.sortBy(\(_, _, time1) (_, _, time2) -> time1 `compare` time2) $ 
              List.filter (\(pid, serviceProfile, time) -> serviceProfile == sP) rsl
  case spl of 
    [] -> queryAnyAvailableService server sP
    h : _ -> return . Just $ h  






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
    $ List.filter(\(prId, _,  _) -> prId /= processId) remoteServiceQueue
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

