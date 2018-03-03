module Data.Zya.Utils.IPC where

import Control.Monad.Trans
import Control.Exception(SomeException(..), handle)
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


domainSocket :: (MonadIO m) => FilePath -> m Socket
domainSocket filePath = do 
  sock <- liftIO $ socket AF_UNIX Stream 0 
  liftIO $ debugMessage $ Text.pack $ "Opening socket " <> (show filePath)
  _ <- liftIO $ setSocketOption sock KeepAlive 0
  _ <- liftIO $ connect sock (SockAddrUnix filePath)
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


monadIOHandle excHandler safeFunction = liftIO $ handle excHandler safeFunction
sendMessageWithSockets :: (MonadIO m) => Socket -> Value -> m (Maybe Value)
sendMessageWithSockets sock request =
  monadIOHandle
    (\e@(SomeException exception) -> 
        (errorMessage $ T.pack $ "Exception " <> show e) >> return Nothing) $ do 
    let requestBS = L.toStrict . encode $ request 
    debugMessage  $ T.pack $ " Sending " <>  (show $ Data.ByteString.length requestBS)
    _ <- liftIO $ Network.Socket.ByteString.send sock requestBS
    msg <- liftIO $ Network.Socket.ByteString.recv sock defaultBufferSize
    return . decode . fromStrict $ msg


-- | Print closing handle.
closeHandle :: (MonadIO m) => Socket -> m ()
closeHandle h = do 
  liftIO $ debugMessage $ Text.pack $ "Closing handle " <> show h
  liftIO . close $ h



defaultBufferSize :: Int 
defaultBufferSize = 100 * 1024 * 1024
