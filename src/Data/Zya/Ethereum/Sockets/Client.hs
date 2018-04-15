{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Zya.Ethereum.Sockets.Client 
(
  BrowseBlocks.browseBlocksAsync
  , queryTransactionIO
  , sendTransactionMain
)

where

import Control.Exception(bracket)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans(liftIO)
import Data.Aeson
import Data.Monoid
import Data.Text as Text
import Data.Zya.Ethereum.Internal.Types.RPCRequest
import Data.Zya.Ethereum.Sockets.BrowseBlocks as BrowseBlocks
import Data.Zya.Ethereum.Sockets.GethApplication as GethApplication
import Data.Zya.Utils.IPC
import Data.Zya.Utils.JsonRPC
import Data.Zya.Utils.Logger(debugMessage)
import Network.Socket(Socket)
import qualified Data.Text as T
import System.IO

-- | Send a transaction request.
sendTransactionMain :: (MonadIO m) => FilePath -> String -> TransactionRequest -> m ()
sendTransactionMain filePath addressId transactionRequest = 
  mapM 
    (sendTransactionIOWithBracket filePath addressId) [transactionRequest] >> return ()


queryTransactionIO :: 
    (Traversable t, MonadIO m) => FilePath -> String -> t EthData -> m ()
queryTransactionIO filePath addressId txId = 
  mapM (GethApplication.queryTransactionWithBracket filePath addressId) txId >> return ()

-- | Balance transfers
type TransactionRequest = (Address, Address, Integer, Integer, Integer, Integer, Integer)
sendTransaction :: MonadIO m => Socket -> RequestId -> TransactionRequest -> m (Maybe Value)
sendTransaction aSocket aRequestId (fromAddress', toAddress', gas', gasPrice', value', contractData', nonce') = do 
  let request = eth_sendTransaction 
                  aRequestId fromAddress' toAddress'
                  gas' gasPrice' value' 
                  (Address $ convertIntToHex contractData') nonce' 
  result' <- sendMessageWithSockets aSocket request 
  debugMessage $ T.pack $ "SendTransaction to " <> (show request) <>  " " <> (show result') 
  return result'


sendTransactionInSession :: 
  (MonadReader SessionConfig m, MonadState SessionState m, MonadIO m) => 
      TransactionRequest -> m (Maybe Value) 
sendTransactionInSession r = do 
  sessionState <- get 
  cfg <- ask
  let socket = sessionSocket cfg 
  let requestId = nextRequestId sessionState 
  liftIO $ sendTransaction socket requestId r

-- runSendTransaction
runSendTransaction :: 
  (MonadIO m) => 
    Socket -> String -> TransactionRequest -> m (Maybe Value, SessionState) 
runSendTransaction socket accountAddress' transactionRequest = do 
  -- Overwrite to avoid filling up space.
  defFileHandle <- liftIO $ openFile "defautSendTransaction.csv" WriteMode 
  let 
    config = SessionConfig 1 socket "tbd" defFileHandle (read accountAddress') 0 0
    state' = SessionState 0 Nothing 
  liftIO $ runStateT (runReaderT (runA (sendTransactionInSession transactionRequest)) config) state'


sendTransactionIOWithBracket :: 
  (MonadIO m) => FilePath -> String -> TransactionRequest -> m (Maybe Value, SessionState)
sendTransactionIOWithBracket aFilePath accountAddress' transactionRequest = do 
  liftIO $ bracket (domainSocket aFilePath) (\h -> closeHandle h) $ \socket -> do 
          liftIO $ debugMessage $ Text.pack $ "Processing transaction " <> (show transactionRequest)
          result' <- runSendTransaction socket accountAddress' transactionRequest
          return result'



