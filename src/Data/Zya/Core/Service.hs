module Data.Zya.Core.Service
    (
        -- * The supported service profiles
        ServiceProfile(..)
        -- New server
        , newServer
        , Server

    )
where 
import Data.Map as Map
import Control.Concurrent.STM
import Control.Distributed.Process
import Data.Text 

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
}

newServer :: Process Server 
newServer =  
    liftIO $ do 
        localClients <- newTVarIO (Map.empty) 
        remoteClientMap <- newTVarIO (Map.empty) 
        localWriterMap <- newTVarIO (Map.empty) 
        remoteWriterMap <- newTVarIO (Map.empty)
        serviceMap <- newTVarIO (Map.empty) 
        statistics <- newTVarIO (Map.empty)
        return $ Server {
            localClients = localClients
            , remoteClients = remoteClientMap 
            , localWriters = localWriterMap 
            , remoteWriters = remoteWriterMap
            , services = serviceMap 
            , statistics = statistics
    }

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
    deriving(Show)


