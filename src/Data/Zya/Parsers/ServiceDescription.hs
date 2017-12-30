module Data.Zya.Parsers.ServiceDescription where 
import Data.Generics 
import Text.ParserCombinators.Parsec
import Data.Text
import Control.Monad.Cont 
import Control.Monad.Logic

{-data ServiceProfile =
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
-}


{--
    [|
    start 10 webservers 
    start 20 postgres database server
    start 10 webservers 
    |]

-}

type HostName = Text
data DBType = Postgres | MySql | Riak | Memory
data DBConnection = DBConnection {_databaseType :: DBType, _dbConnUrl :: Text}
type ComputationType = Text
newtype PortNumber = PortNumber {_unPort :: Int}

data CloudService = 
    WebServer HostName PortNumber 
    | Reader DBConnection
    | Writer DBConnection
    | QueryAllocator 
    | Terminator 
    | TestWriter
    | ComputeNode ComputationType
    --deriving(Show, Generic, Typeable, Eq, Ord)

create :: Parser String 
create = string "Create"

