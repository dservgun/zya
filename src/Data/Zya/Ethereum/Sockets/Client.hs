{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Ethereum.Sockets.Client where

import Control.Concurrent(forkIO)
import Control.Monad(forever, unless)
import Control.Monad.Trans(liftIO)
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
type Transaction = Text
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




