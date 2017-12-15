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
  return $ decode . encode . decodeUtf8 $ msg

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

