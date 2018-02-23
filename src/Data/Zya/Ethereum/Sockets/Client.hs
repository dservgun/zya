{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Zya.Ethereum.Sockets.Client 
(
  browseBlocks
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
import Data.Text.IO as TextIO
import Data.Text.Encoding
import Data.Text.Lazy (toStrict)
import Data.Zya.Ethereum.Internal.Types.Common
import Data.Zya.Ethereum.Internal.Types.RPCRequest
import Data.Zya.Ethereum.Internal.Types.RPCResponse
import Data.Zya.Utils.IPC
import Data.Zya.Utils.JsonRPC
import Data.Zya.Utils.FileUtil
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
import Data.Zya.Utils.Logger(setup, debugMessage, infoMessage, errorMessage)

import Text.Printf 



sendMessage :: Value -> FilePath -> IO (Maybe Value)
sendMessage aValue aFilePath = do 
  let packS = Text.pack
  sock <- domainSocket aFilePath 
  System.IO.putStrLn(T.unpack $ decodeUtf8 $ L.toStrict $ encode $ aValue)
  handle (\e@(SomeException e1) -> errorMessage . packS $ (show e) <> " : Error") $ Network.Socket.ByteString.sendAll sock (L.toStrict $ encode $ aValue)
  msg <- Network.Socket.ByteString.recv sock 4096
  System.IO.putStrLn(show msg)
  System.IO.putStrLn(Text.unpack $ decodeUtf8 msg)
  return $ decode . fromStrict $ msg


type Account = Text 


type RequestId = Int
type BlockIdAsInt = Integer

-- Get the block by a specific hash block. 
-- these are internal functions, need to be moved around.
getBlockByHash :: Socket -> RequestId -> BlockQuantity -> IO (Result BlockByHash)
getBlockByHash aSocket aRequestId aBlockId = do
  let details = True -- This gets all the transaction details for a block. 
  -- how large can a block get?
  let request = eth_getBlockByNumber aRequestId aBlockId details
  result <- sendMessageWithSockets aSocket request
  debugMessage $ T.pack $ "Get block by hash " <> " " <> (show request)
  return $ joinResponse $ fmap fromJSON result

getTransactionByHash :: 
  (MonadIO m) => Socket -> RequestId -> EthData -> m (Result Transaction)
getTransactionByHash aSocket aRequestId aTransactionHash = do  
  let request = eth_getTransactionByHash aRequestId aTransactionHash 
  result <- sendMessageWithSockets aSocket request 
  debugMessage $ T.pack $  " Transaction returned " <> show result
  return $ joinResponse $ fmap fromJSON result


getTransactions :: (Result BlockByHash) -> [Transaction]
getTransactions aBlockByHash = 
    case aBlockByHash of 
      Success a -> transactions a
      Error s -> []

filterTransactions :: Integer -> [Transaction] -> [Transaction]
filterTransactions address transactions = 
  Prelude.filter(\a -> from a == address || to a == address) transactions


--- Design
--- The application session contains a read only socket
--- the last request is the previous state
--- The tell writes to a list of transactions. 

data SessionConfig = SessionConfig {
  startRequestId :: Integer
  , sessionSocket :: Socket
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

newtype SessionRequest = SessionRequest {_unReq :: Text} deriving (Show)
newtype SessionResponse = SessionResponse {_unRes :: Text} deriving(Show)


joinResponse :: Maybe(Result a) -> Result a
joinResponse (Just a) = a 
joinResponse Nothing = Error "No response.."

-- 1. Send a sync request.
-- 2. If the response is a success, 
-- 3. Send a request to query all transactions for the block.
-- 4. If the transactions match the address, add them to the result, 
-- 5. If not, decrement the block and repeat, until there are no blocks to process.


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

  return block


getAllFilteredTransactionsForAddress :: 
  (MonadReader SessionConfig m, MonadState SessionState m, MonadIO m) => m [Transaction]
getAllFilteredTransactionsForAddress = do 
  sessionState <- get 
  cfg <- ask
  let socket = sessionSocket cfg 
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
  return $ (Prelude.concat transactionList)



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
  let 
    config = SessionConfig 1 socket (read accountAddress) 0 0
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



runGethSession :: Socket -> String -> (Integer, Integer) -> IO([Transaction], SessionState)
runGethSession socket accountAddress (start, numberOfBlocks) = 
  action `catch` (\e@(SomeException c) -> do 
      errorMessage $ Text.pack (show e)
      return (([], (SessionState 0 Nothing))))
  where 
    action = do 
      let    
        config = SessionConfig 1 socket (read accountAddress) start (start + numberOfBlocks)
        state = SessionState 0 Nothing
      debugMessage $ Text.pack ("finalInterface " <> show config) 
      runStateT (runReaderT (runA gethSession) config) state


runGethSessionWithAccounts :: FilePath -> [String] -> (Integer, Integer) -> IO[([Transaction], SessionState)]
runGethSessionWithAccounts aFilePath accountAddresses (start, end) = do
  x <- bracket (domainSocket aFilePath) (\h -> closeHandle h) $ \socket -> do
          accts <- return accountAddresses 
          debugMessage $ Text.pack $ "Processing block range " <> (show start) <> " --> " <> (show end)
          result <- 
            mapM (\a -> do 
                          debugMessage $ Text.pack $ show a
                          runGethSession socket a (start, end)) accts
          debugMessage $ Text.pack $ 
              "Processing block range " 
              <> (show start) <> " ----> " 
              <> (show end) <>  " " <> (show result)
          return $ result
  return x

{-- | 
  Browse for an account with account address between start and end.
  Function uses bracket to deal with any errors on the handle.
--}
runGethSessionWithBracket :: FilePath -> String -> (Integer, Integer) -> IO[([Transaction], SessionState)]
runGethSessionWithBracket aFilePath accountAddress (start, end) = 
  runGethSessionWithAccounts aFilePath [accountAddress] (start, end)

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


-- | Print transactions as text.
printTransactions :: [Transaction] -> OutputFormat -> [Text]
printTransactions transactionList (a@(CSV ",")) = 
  Prelude.map (\t -> transactionOutput t a) transactionList


{-- | 
  * ipcPath : the path to the ethereum node. 
  * outputFile : the output.
  * accountsFile : a file that lists accounts to browse for.
--}
browseBlocks :: FilePath -> FilePath -> FilePath -> (Integer, Integer, Integer) -> IO ()
browseBlocks ipcPath outputFile accountsFile (start, range, defaultBlocks) = do
  let unfoldList = chunkBlocks(start, range, defaultBlocks)
  accountAddresses <- readInputLines accountsFile
  bracket(openFile outputFile WriteMode)(hClose) (\h -> do
        TextIO.hPutStrLn h "Hash, Currency, target-address, sender-address, value, gasPrice, gas")
  transactions <- 
      mapM (\x -> 
        runGethSessionWithAccounts 
          ipcPath
          accountAddresses
          (x, defaultBlocks)) unfoldList
  debugMessage $ "Processed transactions..."
  bracket(openFile outputFile AppendMode)(hClose) (\h -> do
        --debugMessage $ Text.pack $ show transactions  
        TextIO.hPutStrLn h $ Text.unlines $ 
            printTransactions 
              (Prelude.concat $ fst <$> Prelude.concat transactions) 
              (CSV ",")
        )
  return ()


-- | Print closing handle.
closeHandle :: (MonadIO m) => Socket -> m ()
closeHandle h = do 
  liftIO $ debugMessage $ Text.pack $ "Closing handle " <> show h
  liftIO . close $ h

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
  let 
    config = SessionConfig 1 socket (read accountAddress) 0 0
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


