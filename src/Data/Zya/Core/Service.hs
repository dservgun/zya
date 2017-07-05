module Data.Zya.Core.Service
    (
        -- * The main service 
        service
        -- * The supported service profiles
        , ServiceProfile(..)
        -- **

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
    * localClientMap - For each client identifier, list of topics and their read positions.
    * remoteClientMap - For each process id the state of the topics.
    * localWriterMap - Write position for a topic.
    * remoteWriterMap - Write position for a topic. 
    * serviceMap - A map of the services running on the network.
    ** Note: There should exist only one write position per topic.

 -}
data Server = Server {
    localClientMap :: TVar (Map ClientIdentifier [ClientState])
    , remoteClientMap :: TVar (Map (ProcessId, ClientIdentifier) [ClientState])
    , localWriterMap :: TVar (Map Topic Integer)
    , remoteWriterMap :: TVar (Map (ProcessId, Topic) Integer)
    ,  serviceMap :: TVar (Map (ProcessId, ServiceProfile) Integer)
    , statistics :: TVar (Map ProcessId ([Request], [Response]))
}

type ServiceRange = (Int, Int) 

{- | A typical default configuration.
-}
defaultSimpleConfiguration :: [(ServiceProfile, ServiceRange)]
defaultSimpleConfiguration = [(WebServer, (3, 10)), (DatabaseServer, (3, 10)), 
                        (Reader, (3, 10)), 
                        (Writer, (3, 10))]

{- | Services that can be started on a cloud. -}
data ServiceProfile = 
    WebServer | DatabaseServer | Reader | Writer
    deriving(Show)

newtype Service = Service {unService :: ServiceProfile -> [NodeId] -> Process[NodeId]}

service :: Service -> Process[NodeId]
service = undefined