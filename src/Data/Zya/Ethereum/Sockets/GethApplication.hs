{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Zya.Ethereum.Sockets.GethApplication
  (
    -- * Application
    EthereumSessionApp
    -- * runner
    , runA
    -- * Configuration and state.
    , SessionConfig(..)
    , SessionState(..) 
    , SessionStatus
    , SessionRequest
    , SessionResponse
    , queryTransaction
    , queryTransactionWithBracket
    , RequestId
  )
 where 

import Control.Exception(bracket)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans(liftIO)
import Data.Aeson
import Data.Monoid
import Data.Zya.Ethereum.Internal.Types.Common
import Data.Zya.Ethereum.Internal.Types.RPCRequest
import Data.Zya.Utils.IPC (sendMessageWithSockets, domainSocket, closeHandle)
import Data.Zya.Utils.JsonRPC
import Network.Socket(Socket)
import Data.Zya.Utils.Logger(debugMessage)
import qualified Data.Text as T
import System.IO

type RequestId = Int

--- Design
--- The application session contains a read only socket
--- the last request is the previous state

data SessionConfig = SessionConfig {
  startRequestId :: Integer
  , sessionSocket :: Socket
  , socketPath :: FilePath  
  , outputFileHandle :: Handle
  , accountAddress :: Integer
  , startBlock :: Integer
  , endBlock :: Integer
} deriving(Show)

data SessionState = SessionState {
  nextRequestId :: Int
  , curBlock :: Maybe Integer
} deriving(Show)

newtype EthereumSessionApp a = EthereumSessionApp {
  runA :: ReaderT SessionConfig (StateT SessionState IO) a
} deriving(Functor, Applicative, Monad, MonadIO, MonadReader SessionConfig, MonadState SessionState)

data SessionStatus = EthSuccess | EthFailure deriving (Show)

newtype SessionRequest = SessionRequest {_unReq :: T.Text} deriving (Show)
newtype SessionResponse = SessionResponse {_unRes :: T.Text} deriving(Show)


queryTransactionByHash :: 
  (MonadReader SessionConfig m, MonadState SessionState m, MonadIO m) => EthData -> m (Result Transaction)
queryTransactionByHash transactionHash = do 
  sessionState <- get 
  cfg <- ask 
  let socket = sessionSocket cfg 
  let requestId = nextRequestId sessionState 
  liftIO $ getTransactionByHash socket requestId transactionHash 


queryTransaction :: 
  (MonadIO m) => Socket -> String -> T.Text -> m (Result Transaction, SessionState) 
queryTransaction socket accountAddress' transactionHash = do 
  defFileHandle <- liftIO $ openFile "defaultQueryTransaction.csv" WriteMode
  let 
    config = SessionConfig 1 socket "tbd" defFileHandle (read accountAddress') 0 0
    state' = SessionState 0 Nothing 
  liftIO $ 
    runStateT (runReaderT (runA (queryTransactionByHash transactionHash)) config) state'


queryTransactionWithBracket :: 
  (MonadIO m) => FilePath -> String -> EthData -> m (Result Transaction, SessionState)
queryTransactionWithBracket aFilePath accountAddress' transactionHash = do 
  liftIO $ 
    bracket (domainSocket aFilePath) (\h -> closeHandle h ) $ \socket -> do 
          debugMessage $ T.pack $ "Processing transaction " <> (show transactionHash)
          result <- queryTransaction socket accountAddress' transactionHash
          return result

getTransactionByHash :: 
  (MonadIO m) => Socket -> RequestId -> EthData -> m (Result Transaction)
getTransactionByHash aSocket aRequestId aTransactionHash = do  
  let request = eth_getTransactionByHash aRequestId aTransactionHash 
  result <- sendMessageWithSockets aSocket request 
  debugMessage $ T.pack $  " Transaction returned " <> show result
  return $ joinResponse $ fmap fromJSON result
