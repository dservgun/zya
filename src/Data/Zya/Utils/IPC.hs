module Data.Zya.Utils.IPC where

import Data.Aeson
import Data.ByteString hiding (hPutStrLn, hGetLine)
import Data.ByteString.Lazy as L hiding(hGetContents)
import Data.Monoid((<>))
import Data.Text as Text
import Data.Zya.Utils.Logger(setup, debugMessage, infoMessage, errorMessage)
import Network.BSD 
import Network.Socket
import Network.Socket.ByteString
import qualified Data.Text as T
import qualified Data.Text.IO as T 
import System.IO(Handle, IOMode(..), hSetBuffering, BufferMode(..))
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Handler.Syslog
import System.Log.Logger


domainSocket :: FilePath -> IO (Socket) 
domainSocket filePath = do 
  sock <- socket AF_UNIX Stream 0 
  setSocketOption sock KeepAlive 0
  connect sock (SockAddrUnix filePath)
  return sock

plainOldSocket :: HostName -> ServiceName -> IO Socket
plainOldSocket hostName portNumber = do 
    addrInfos <- getAddrInfo Nothing (Just hostName) (Just portNumber)
    -- Empty list will throw an exception, we dont need 
    -- to check for a safe head in this case.
    let serverAddr = Prelude.head addrInfos
    sock <- socket(addrFamily serverAddr) Stream defaultProtocol 
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serverAddr)
    infoMessage (Text.pack $ "Connected to " <> (show hostName) <> ":" <> (show portNumber))
    return sock



sendMessageWithSockets :: Socket -> Value -> IO (Maybe Value)
sendMessageWithSockets sock request = do
  let requestBS = L.toStrict . encode $ request 
  Prelude.putStrLn $ "Sending " <> (show requestBS)
  debugMessage  $ T.pack $ " Sending " <>  (show $ Data.ByteString.length requestBS)
  _ <- Network.Socket.ByteString.send sock requestBS
  msg <- Network.Socket.ByteString.recv sock defaultBufferSize
  Prelude.putStrLn $ show msg
  debugMessage $ T.pack $ "Received " <> (show $ Data.ByteString.length msg)
  return . decode . fromStrict $ msg




defaultBufferSize :: Int 
defaultBufferSize = 100 * 1024 * 1024
