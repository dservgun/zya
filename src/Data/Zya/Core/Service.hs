{-# LANGUAGE DeriveGeneric #-}
module Data.Zya.Core.Service
    (
        -- * The supported service profiles
        ServiceProfile(..)
        -- New server
        , newServer
        , Server(..) -- todo: How to deal with exposing types..need to review rwh.

    )
where 
import Data.Monoid((<>))
import Data.Map as Map
import Data.List as List
import Control.Monad
import Control.Concurrent.STM
import Control.Distributed.Process
import Data.Text 
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
    , proxyChannel :: TChan(Process())
    , myProcessId :: TVar (Maybe ProcessId)
}

newServer :: Process Server 
newServer =  
    liftIO $ do 
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
            , proxyChannel = proxyChannel
            , myProcessId = initProcessId 
        }




{- | 
  Given a service profile, return the count of services and the ProcessId for the service.

-}
queryService :: Server -> ServiceProfile -> STM[(ProcessId, ServiceProfile, Integer)]
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
    deriving(Show, Generic, Typeable)

instance Binary ServiceProfile

{-| 
  A global map indicating if a particular service is a singleton, ideally a leader should fix the 
  need for this map. 
-}
isSingleton :: ServiceProfile -> Bool
isSingleton TopicAllocator = True
isSingleton _ = False