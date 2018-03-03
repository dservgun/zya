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

import Control.Applicative
import Control.Concurrent(forkIO)
import Control.Exception(bracket, handle, SomeException(..), catch)
import Control.Monad(forever, unless)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans(liftIO)
import Data.Aeson
import Data.ByteString hiding (hPutStrLn, hGetLine)
import Data.ByteString.Lazy as L hiding(hGetContents)
import Data.Monoid
import Data.Text as Text
import Data.Text.Encoding
import Data.Text.IO as TextIO
import Data.Text.Lazy (toStrict)
import Data.Zya.Ethereum.Internal.Types.Common
import Data.Zya.Ethereum.Internal.Types.RPCRequest
import Data.Zya.Ethereum.Internal.Types.RPCResponse
import Data.Zya.Ethereum.Internal.Types.Transaction
import Data.Zya.Ethereum.Sockets.BrowseBlocks as BrowseBlocks
import Data.Zya.Ethereum.Sockets.GethApplication
import Data.Zya.Utils.FileUtil
import Data.Zya.Utils.IPC
import Data.Zya.Utils.JsonRPC
import Data.Zya.Utils.Logger(setup, debugMessage, infoMessage, errorMessage)
import Network.Socket(Socket, close)
import Network.Socket.ByteString
import qualified Data.Text as T
import qualified Data.Text.IO as T 
import System.IO
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Handler.Syslog
import System.Log.Logger
import Text.Printf
import Text.Printf 



sendMessage :: Value -> FilePath -> IO (Maybe Value)
sendMessage aValue aFilePath = do 
  let packS = Text.pack
  sock <- domainSocket aFilePath 
  handle (\e@(SomeException e1) -> errorMessage . packS $ (show e) <> " : Error") $ Network.Socket.ByteString.sendAll sock (L.toStrict $ encode $ aValue)
  msg <- Network.Socket.ByteString.recv sock 4096
  return $ decode . fromStrict $ msg




gethSession :: 
      (MonadReader SessionConfig m, MonadState SessionState m, MonadIO m) => m [Transaction]
gethSession = do 
  cfg <- ask
  sessionState <- get
  let socket = sessionSocket cfg
  let nReq = nextRequestId sessionState 
  --TODO : Cleanup
  s2 <- get
  block <- getAllFilteredTransactionsForAddress

  return [] -- TODO fix this.



-- TODO dead code removal.
getAllFilteredTransactionsForAddress :: 
  (MonadReader SessionConfig m, MonadState SessionState m, MonadIO m) => m ()
getAllFilteredTransactionsForAddress = do 
  sessionState <- get 
  cfg <- ask
  let socket = sessionSocket cfg 
  let outputFileH = outputFileHandle cfg
  let accountAddr = accountAddress cfg
  let start = startBlock cfg
  let end = endBlock cfg
  let requestId = nextRequestId sessionState 
  let currentBlockId = curBlock sessionState
  result <- mapM (\x -> do 
    prev <- get
    modify (\s -> s {nextRequestId = (nextRequestId prev) + 1})
    s <- get
    let reqId = nextRequestId s
    liftIO $ debugMessage 
      $ T.pack $ "Request id " 
        <> (show reqId) <>  " " <> (show start) <> " - " <> (show end) <> " - "
        <> (show x)
    liftIO $ getBlockByHash socket reqId (BlockId x)
    ) [start .. end]
  liftIO $ debugMessage $ T.pack $ "Blocks returned "
  let transactionList = Prelude.map (\x -> 
                            filterTransactions accountAddr $ getTransactions x
                        ) result
  liftIO $ debugMessage $ T.pack $ "Returning transaction lists..." 
            <> (show (Prelude.length transactionList))
  let transactionTexts = printTransactions (Prelude.concat transactionList) (CSV ",")
  let textToBePrinted = Text.unlines transactionTexts
  if (textToBePrinted /= "") then do
    liftIO $ TextIO.hPutStrLn outputFileH $ textToBePrinted
    liftIO $ System.IO.hFlush outputFileH
    return ()
  else
    return ()




queryTransactionByHash :: 
  (MonadReader SessionConfig m, MonadState SessionState m, MonadIO m) => EthData -> m (Result Transaction)
queryTransactionByHash transactionHash = do 
  sessionState <- get 
  cfg <- ask 
  let socket = sessionSocket cfg 
  let requestId = nextRequestId sessionState 
  liftIO $ getTransactionByHash socket requestId transactionHash 


queryTransaction :: 
  (MonadIO m) => Socket -> String -> Text -> m (Result Transaction, SessionState) 
queryTransaction socket accountAddress transactionHash = do 
  defFileHandle <- liftIO $ openFile "defaultQueryTransaction.csv" WriteMode
  let 
    config = SessionConfig 1 socket "tbd" defFileHandle (read accountAddress) 0 0
    state = SessionState 0 Nothing 
  liftIO $ runStateT (runReaderT (runA (queryTransactionByHash transactionHash)) config) state


queryTransactionWithBracket :: 
  (MonadIO m) => FilePath -> String -> EthData -> m (Result Transaction, SessionState)
queryTransactionWithBracket aFilePath accountAddress transactionHash = do 
  liftIO $ 
    bracket (domainSocket aFilePath) (\h -> closeHandle h ) $ \socket -> do 
          debugMessage $ Text.pack $ "Processing transaction " <> (show transactionHash)
          result <- queryTransaction socket accountAddress transactionHash
          return result

queryTransactionIO :: 
    (Traversable t, MonadIO m) => FilePath -> String -> t EthData -> m ()
queryTransactionIO filePath addressId txId = 
  mapM (queryTransactionWithBracket filePath addressId) txId >> return ()


runGethSession :: Socket -> Handle -> String -> (Integer, Integer) -> IO([Transaction], SessionState)
runGethSession socket fHandle accountAddress (start, numberOfBlocks) = 
  action `catch` (\e@(SomeException c) -> do 
      errorMessage $ Text.pack (show e)
      return (([], (SessionState 0 Nothing))))
  where 
    action = do 
      let    
        config = SessionConfig 1 socket "tbd" fHandle (read accountAddress) start (start + numberOfBlocks)
        state = SessionState 0 Nothing
      debugMessage $ Text.pack ("finalInterface " <> show config) 
      runStateT (runReaderT (runA gethSession) config) state



{-- | 
  Given a starting block, step through blocks using the block size.
  For example, 
  >chunkBlocks (1, 9, 1) = [1 .. 10]
  >chunkBlocks (1, 9, 2) = [1, 3, 5, 7, 9]
  >chunkBlocks (1, 9, 3) = [1, 4, 7, 10] 
--}
chunkBlocks :: (Ord a, Num a) => (a, a, a) -> [a] 
chunkBlocks (start, numberOfBlocks, defaultBlockSize) =
    Prelude.takeWhile 
      (\x -> x <= (start + numberOfBlocks))  $ Prelude.iterate (+ defaultBlockSize) start


-- | Balance transfers
type TransactionRequest = (Address, Address, Integer, Integer, Integer, Integer, Integer)
sendTransaction :: MonadIO m => Socket -> RequestId -> TransactionRequest -> m (Maybe Value)
sendTransaction aSocket aRequestId (fromAddress, toAddress, gas, gasPrice, value, contractData, nonce) = do 
  let request = eth_sendTransaction 
                  aRequestId fromAddress toAddress
                  gas gasPrice value 
                  (Address $ convertIntToHex contractData) nonce 
  result <- sendMessageWithSockets aSocket request 
  debugMessage $ T.pack $ "SendTransaction to " <> (show request) <>  " " <> (show result) 
  return result


sendTransactionInSession :: 
  (MonadReader SessionConfig m, MonadState SessionState m, MonadIO m) => 
      TransactionRequest -> m (Maybe Value) 
sendTransactionInSession r@(fromAddress, toAddress, gas, gasPrice, value, contractData, nonce) = do 
  sessionState <- get 
  cfg <- ask
  let socket = sessionSocket cfg 
  let requestId = nextRequestId sessionState 
  liftIO $ sendTransaction socket requestId r

-- runSendTransaction
runSendTransaction :: 
  (MonadIO m) => 
    Socket -> String -> TransactionRequest -> m (Maybe Value, SessionState) 
runSendTransaction socket accountAddress transactionRequest = do 
  -- Overwrite to avoid filling up space.
  defFileHandle <- liftIO $ openFile "defautSendTransaction.csv" WriteMode 
  let 
    config = SessionConfig 1 socket "tbd" defFileHandle (read accountAddress) 0 0
    state = SessionState 0 Nothing 
  liftIO $ runStateT (runReaderT (runA (sendTransactionInSession transactionRequest)) config) state


sendTransactionIOWithBracket :: 
  (MonadIO m) => FilePath -> String -> TransactionRequest -> m (Maybe Value, SessionState)
sendTransactionIOWithBracket aFilePath accountAddress transactionRequest = do 
  liftIO $ bracket (domainSocket aFilePath) (\h -> closeHandle h) $ \socket -> do 
          liftIO $ debugMessage $ Text.pack $ "Processing transaction " <> (show transactionRequest)
          result <- runSendTransaction socket accountAddress transactionRequest
          return result


-- | Send a transaction request.
sendTransactionMain :: (MonadIO m) => FilePath -> String -> TransactionRequest -> m ()
sendTransactionMain filePath addressId transactionRequest = 
  mapM 
    (sendTransactionIOWithBracket filePath addressId) [transactionRequest] >> return ()


