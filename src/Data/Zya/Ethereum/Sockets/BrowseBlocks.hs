{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Zya.Ethereum.Sockets.BrowseBlocks where 

import Control.Exception (bracket, handle, SomeException(..), catch)
import Control.Monad (forever, unless)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Monoid
import Data.Text.IO as TextIO
import Data.Zya.Ethereum.Internal.Bookeeping.ReconTransaction
        (ReconTransaction, reconTransactions, toCSV, address)
import Data.Zya.Ethereum.Internal.Types.Common
import Data.Zya.Ethereum.Internal.Types.RPCRequest
import Data.Zya.Ethereum.Internal.Types.RPCResponse
import Data.Zya.Ethereum.Internal.Types.Transaction
import Data.Zya.Ethereum.Sockets.GethApplication
import Data.Zya.Utils.FileUtil (readInputLines)
import Data.Zya.Utils.IPC (sendMessageWithSockets, domainSocket, closeHandle)
import Data.Zya.Utils.JsonRPC
import Data.Zya.Utils.Logger (debugMessage, infoMessage, errorMessage)
import Network.Socket (Socket, close)
import Network.Socket.ByteString
import qualified Data.Text as T
import qualified Data.Text.IO as T 
import System.IO
import Text.Printf


type RequestId = Int
type BlockIdAsInt = Integer
type Account = T.Text 


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

{-- | 
  * ipcPath : the path to the ethereum node. 
  * outputFile : the output.
  * accountsFile : a file that lists accounts to browse for.
--}
browseBlocks :: 
  FilePath -> FilePath -> FilePath -> FilePath -> (Integer, Integer, Integer) -> 
    IO [([Transaction], SessionState)]
browseBlocks ipcPath outputFile accountsFile reconFile (start, range, defaultBlocks) = do
  let unfoldList = chunkBlocks(start, range, defaultBlocks)
  accountAddresses <- readInputLines accountsFile
  reconTransactions <- reconTransactions reconFile
  transactions <- bracket(openFile outputFile WriteMode)(hClose) (\outputFileHandle -> do
        TextIO.hPutStrLn outputFileHandle "Hash, Currency, target-address, sender-address, value, gasPrice, gas, intentAddress, intentAmount"
        transactions <- 
          mapM (\x -> 
            runGethSessionWithAccounts 
              ipcPath
              outputFileHandle
              reconTransactions
              accountAddresses          
              (x, defaultBlocks - 1)) unfoldList
        debugMessage $ "Processed transactions..."
        TextIO.hPutStrLn outputFileHandle $ "End processing"
        return transactions
        )
  trans <- return . (Prelude.concat) $ fmap fst transactions
  
  return transactions




runGethSessionWithAccounts :: FilePath -> Handle -> [ReconTransaction] -> [String] -> (Integer, Integer) -> IO([Transaction], SessionState)
runGethSessionWithAccounts aFilePath outputFileHandle reconTransactions accountAddresses (start, end) = do
  x <- bracket (domainSocket aFilePath) (\h -> closeHandle h) $ \socket -> do
          accts <- return accountAddresses 
          debugMessage $ T.pack $ "Processing block range " <> (show start) <> " --> " <> (show end)
          result <- runGethSessionMultipleAccounts socket outputFileHandle aFilePath reconTransactions accts (start, end)
          debugMessage $ T.pack $ 
              "Processed block range " 
              <> (show start) <> " ----> " 
              <> (show end)
          return $ result
  return x



runGethSessionMultipleAccounts socket fHandle ipcPath reconTransactions accountAddresses (start, numberOfBlocks) = 
  action `catch` (\e@(SomeException c) -> do 
      errorMessage $ T.pack (show e)
      return (([], (SessionState 0 Nothing))))
  where 
    action = do 
      let    
        config = SessionConfig 1 socket ipcPath fHandle (0) start (start + numberOfBlocks)
        state = SessionState 0 Nothing
      debugMessage $ T.pack ("finalInterface " <> show config) 
      runStateT (runReaderT (runA $ gethSessionAccounts reconTransactions accountAddresses) config) state


gethSessionAccounts :: 
      (MonadReader SessionConfig m, MonadState SessionState m, MonadIO m) => 
        [ReconTransaction] -> [String] -> m [Transaction]
gethSessionAccounts reconTransactions accountAddresses = do 
  cfg <- ask
  sessionState <- get
  let socket = sessionSocket cfg
  let nReq = nextRequestId sessionState 
  --TODO : Cleanup
  s2 <- get
  block <- 
    getAllFilteredTransactionsForAddresses reconTransactions accountAddresses
  return block


printRecons :: [ReconTransaction] -> [Transaction] -> OutputFormat -> [T.Text]
printRecons reconTransactions transactions format = do  
  let 
    c = collectTransactions reconTransactions transactions
  Prelude.map (\(r, t) -> transactionOutput t format <> "," <> toCSV r) c 

collectTransactions :: [ReconTransaction] -> [Transaction] -> [(ReconTransaction, Transaction)]
collectTransactions reconTransactions transactions = 
    [
      (intent, trans) | 
        intent <- reconTransactions
        , trans <- transactions
        , (address intent == to trans)
    ]



filterTransactionForBlock :: 
  (MonadReader SessionConfig m, MonadState SessionState m, MonadIO m) => 
    Socket -> Integer -> m (Result BlockByHash)
filterTransactionForBlock socket blockId = return $ Error "test"
{-  do
    prev <- get
    modify (\s -> s {nextRequestId = (nextRequestId prev) + 1})
    s <- get
    let reqId = nextRequestId s
    liftIO $ debugMessage 
      $ T.pack $ "Request id " 
        <> (show reqId) <>  " " <> (show blockId)
    r <- liftIO $ getBlockByHash socket reqId (BlockId blockId)
    return r
-}      

getAllFilteredTransactionsForAddresses :: 
  (MonadReader SessionConfig m, MonadState SessionState m, MonadIO m) => 
    [ReconTransaction] -> [String] -> m [Transaction]
getAllFilteredTransactionsForAddresses reconTransactions accountAddresses = do 
  sessionState <- get 
  cfg <- ask
  let (socket, ipcPath) = (sessionSocket cfg , socketPath cfg)
  let outputFileH = outputFileHandle cfg
  let (start, end) = (startBlock cfg, endBlock cfg) 
  let (requestId, currentBlockId) = (nextRequestId sessionState , curBlock sessionState)
  result <- mapM (\x ->  filterTransactionForBlock socket x) [start .. end]
  _ <- closeHandle socket
  let transactionList = Prelude.map (\x -> do
                            filterTransactionsA accountAddresses $ getTransactions x
                        ) result
  liftIO $ debugMessage $ T.pack $ "Returning transaction lists..." 
            <> (show (transactionList))
  let transactionTexts = printRecons (reconTransactions) (Prelude.concat transactionList) (CSV ",")
  let textToBePrinted = T.unlines transactionTexts
  if (textToBePrinted /= "") then do
    liftIO $ TextIO.hPutStrLn outputFileH $ textToBePrinted
    liftIO $ System.IO.hFlush outputFileH
    return $ Prelude.concat transactionList
  else
    return []



-- Get the block by a specific hash block. 
-- these are internal functions, need to be moved around.
getBlockByHash :: Socket -> RequestId -> BlockQuantity -> IO (Result BlockByHash)
getBlockByHash aSocket aRequestId aBlockId = do
  let details = True -- This gets all the transaction details for a block. 
  -- how large can a block get?
  let request = eth_getBlockByNumber aRequestId aBlockId details
  result <- sendMessageWithSockets aSocket request
  debugMessage $ T.pack $ "Get block by hash " <> " " <> (show result)
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

filterTransactionsA :: [String] -> [Transaction] -> [Transaction]
filterTransactionsA addresses transactions = 
  Prelude.concat $
    Prelude.map (\a -> filterTransactions (read a) transactions) addresses


