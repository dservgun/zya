{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Zya.Ethereum.Sockets.Client where

import Control.Concurrent(forkIO)
import Control.Monad(forever, unless)
import Control.Monad.Trans(liftIO)
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception(bracket)
import Network.Socket
import Network.Socket.ByteString
import Network.BSD 
import System.IO
import Data.ByteString hiding (hPutStrLn, hGetLine)
import Data.ByteString.Lazy as L hiding(hGetContents)
import Data.Text
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.IO as T 
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import Data.Monoid
import Control.Applicative
import Data.Aeson
import Data.Zya.Ethereum.Internal.Types.Common
import Data.Zya.Ethereum.Internal.Types.RPCRequest
import Data.Zya.Ethereum.Internal.Types.RPCResponse

import Text.Printf 


initializeSysLog :: IO () 
initializeSysLog = do
  l <- openlog "Zya.Ethereum.Client" [PID] USER DEBUG
  updateGlobalLogger rootLoggerName (addHandler l)
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  return ()


errorMessage :: String -> IO ()
errorMessage = errorM rootLoggerName

infoMessage :: String -> IO () 
infoMessage = infoM rootLoggerName

debugMessage :: String -> IO ()
debugMessage = debugM rootLoggerName



defaultBufferSize :: Int 
defaultBufferSize = 10 * 1024

-- Should probably be a generic handle. 
-- should recursively read the message till the buffer is completely
-- read and have an upper cap on the number of bytes we allow 
-- as response. Or i should probably be using conduit.

sendMessageWithSockets :: Socket -> Value -> IO (Maybe Value)
sendMessageWithSockets sock request = do 
  _ <- Network.Socket.ByteString.sendAll sock (L.toStrict $ encode $ request)
  msg <- Network.Socket.ByteString.recv sock defaultBufferSize
  System.IO.putStrLn "After send message with sockets"
  System.IO.putStrLn $ (show msg)
  return . decode . fromStrict $ msg


sendMessage :: Value -> FilePath -> IO (Maybe Value)
sendMessage aValue aFilePath = do 
  sock <- domainSocket aFilePath 
  System.IO.putStrLn(T.unpack $ decodeUtf8 $ L.toStrict $ encode $ aValue)
  _ <- Network.Socket.ByteString.sendAll sock (L.toStrict $ encode $ aValue)
  msg <- Network.Socket.ByteString.recv sock 4096
  System.IO.putStrLn(show msg)
  System.IO.putStrLn(Data.Text.unpack $ decodeUtf8 msg)
  return $ decode . fromStrict $ msg

domainSocket :: FilePath -> IO (Socket) 
domainSocket filePath = do 
  sock <- socket AF_UNIX Stream 0 
  connect sock (SockAddrUnix filePath)
  return sock

entryPoint :: HostName -> ServiceName -> IO (Handle, Socket)
entryPoint hostName portNumber = do 
    addrInfos <- getAddrInfo Nothing (Just hostName) (Just portNumber)
    -- Empty list will throw an exception, we dont need 
    -- to check for a safe head in this case.
    let serverAddr = Prelude.head addrInfos
    sock <- socket(addrFamily serverAddr) Stream defaultProtocol 
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serverAddr)
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h (BlockBuffering Nothing)
    errorMessage ("Connected to " <> (show hostName) <> ":" <> (show portNumber))
    return (h, sock)


type Account = Text 

getTransactionsByAccount :: FilePath -> Account -> Integer -> Integer -> IO [Transaction]
getTransactionsByAccount aFilePath account start end = do 
  domSocket <- domainSocket aFilePath
  let sync = eth_syncResponse $ eth_syncing 12 []
  return $ 
    case sync of 
      Success (SyncResponse s c a b e) -> []
      _ -> []



{-
function getTransactionsByAccount(myaccount, startBlockNumber, endBlockNumber) {
  if (endBlockNumber == null) {
    endBlockNumber = eth.blockNumber;
    console.log("Using endBlockNumber: " + endBlockNumber);
  }
  if (startBlockNumber == null) {
    startBlockNumber = endBlockNumber - 1000;
    console.log("Using startBlockNumber: " + startBlockNumber);
  }
  console.log("Searching for transactions to/from account \"" + myaccount + "\" within blocks "  + startBlockNumber + " and " + endBlockNumber);

  for (var i = startBlockNumber; i <= endBlockNumber; i++) {
    if (i % 1000 == 0) {
      console.log("Searching block " + i);
    }
    var block = eth.getBlock(i, true);
    if (block != null && block.transactions != null) {
      block.transactions.forEach( function(e) {
        if (myaccount == "*" || myaccount == e.from || myaccount == e.to) {
          console.log("  tx hash          : " + e.hash + "\n"
            + "   nonce           : " + e.nonce + "\n"
            + "   blockHash       : " + e.blockHash + "\n"
            + "   blockNumber     : " + e.blockNumber + "\n"
            + "   transactionIndex: " + e.transactionIndex + "\n"
            + "   from            : " + e.from + "\n" 
            + "   to              : " + e.to + "\n"
            + "   value           : " + e.value + "\n"
            + "   time            : " + block.timestamp + " " + new Date(block.timestamp * 1000).toGMTString() + "\n"
            + "   gasPrice        : " + e.gasPrice + "\n"
            + "   gas             : " + e.gas + "\n"
            + "   input           : " + e.input);
        }
      })
    }
  }
}

-}

type RequestId = Int
type BlockIdAsInt = Integer

-- Get the block by a specific hash block. 
-- these are internal functions, need to be moved around.
getBlockByHash :: Socket -> RequestId -> BlockIdAsInt -> IO (Maybe (Result BlockByHash))
getBlockByHash aSocket aRequestId aBlockId = do
  let details = True -- This gets all the transaction details for a block. 
  -- how large can a block get?
  let request = eth_getBlockByNumber aRequestId (BlockId aBlockId) details
  result <- sendMessageWithSockets aSocket request
  return $ fmap fromJSON result

getTransactions :: BlockByHash -> [Transaction]
getTransactions aBlockByHash = transactions aBlockByHash

filterTransactions :: Integer -> [Transaction] -> [Transaction]
filterTransactions address transactions = Prelude.filter(\a -> from a == address || to a == address) transactions


--- Design
--- The application session contains a read only socket
--- the last request is the previous state
--- The tell writes to a list of transactions. 

data SessionConfig = SessionConfig {
  startRequestId :: Integer
  , sessionSocket :: Socket
}

data SessionState = SessionState {
  nextRequestId :: Int
  , currentBlock :: Maybe Integer
}

newtype EthereumSessionApp a = EthereumSessionApp {
  runA :: ReaderT SessionConfig (StateT SessionState IO) a
} deriving(Functor, Applicative, Monad, MonadIO, MonadReader SessionConfig, MonadState SessionState)

data SessionStatus = EthSuccess | EthFailure deriving (Show)

newtype SessionRequest = SessionRequest {_unReq :: Text}
newtype SessionResponse = SessionResponse {_unRes :: Text}


gethSession :: EthereumSessionApp [(SessionRequest, SessionResponse)]
gethSession = do 
  cfg <- ask
  sessionState <- get
  let socket = sessionSocket cfg
  let nReq = nextRequestId sessionState 
  let request = eth_syncing nReq []
  syncResponse <- liftIO $ do 
    sendMessageWithSockets socket request >>= \m -> return $ eth_syncResponse <$> m
  liftIO $ System.IO.putStrLn ("Sync response " <> (show syncResponse))

  put sessionState {nextRequestId = nReq + 1} 
  return [(SessionRequest "test",SessionResponse "test")]


finalInterface :: Socket -> IO([(SessionRequest, SessionResponse)], SessionState)
finalInterface socket = do 
  let 
    config = SessionConfig 1 socket
    state = SessionState 0 Nothing
  runStateT (runReaderT (runA gethSession) config) state


finalInterfaceWithBracket :: FilePath -> IO([(SessionRequest, SessionResponse)], SessionState)
finalInterfaceWithBracket aFilePath = do 
  socket <- domainSocket aFilePath 
  bracket (domainSocket aFilePath) (close) $ \socket -> finalInterface socket


-- Dont commit this, or who cares, really?
testMethod = finalInterfaceWithBracket "/home/dinkarganti/local_test/geth_test.ipc"