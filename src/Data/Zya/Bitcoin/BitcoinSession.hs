{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Zya.Bitcoin.BitcoinSession
(
  generalLedgerApplication
  , HostEndPoint(..)
  , GeneralSessionParameters(..)
  , addresses -- TODO remove this and read from a file.
  , searchTransactions
  , getSingleTransaction
) where

import Control.Exception.Safe
import Control.Lens hiding ((.=))
import Control.Monad(forever, unless)
import Control.Monad.Reader
import Control.Monad.State as State
import Control.Monad.Trans(liftIO)
import Data.Aeson
import Data.Aeson.Lens(key, nth)
import Data.Monoid
import Data.Scientific
import Data.Text as Text
import Text.Printf
import Data.Text.IO as TextIO
import Data.Zya.Bitcoin.Common 
import Data.Zya.Bitcoin.Block
import Data.Zya.Bitcoin.Common as BCommon
import Data.Zya.Bitcoin.JsonRPC hiding (getListReceivedByAddress)
import Data.Zya.Bitcoin.RawTransaction as RawTransaction
import Data.Zya.Bitcoin.Transaction as Transaction
import Data.Zya.Utils.IPC
import Network.HTTP.Client hiding(responseBody)
import Network.Socket
import Network.Wreq as Wreq
import System.Log.Logger
import System.IO
import Data.Zya.Utils.Logger(setup, debugMessage, infoMessage, errorMessage, addFileHandler)
import Data.Zya.Bitcoin.Config



--import Network.Wreq.Internal.Types as WreqTypes

-- Some constants such as transaction count when 
-- accessing the wallet.

data ResultSetSize = ResultSet Int | AllRows deriving (Show)

instance Num ResultSetSize where 
  ResultSet a + ResultSet b = ResultSet (a + b)
  ResultSet a + AllRows = AllRows
  AllRows + _ = AllRows
  fromInteger = ResultSet . fromIntegral


data GeneralSessionParameters = GeneralSessionParameters {
  transactionCount :: Integer
  , resultSize :: ResultSetSize
} deriving(Show)
data HostEndPoint = HostEndPoint {
  hostName :: HostName
  , serviceName :: ServiceName
  , userName :: UserName
  , password :: Password 
} deriving(Show)

createConfig :: String -> String -> String -> SessionConfig 
createConfig userName password port = 
    let 
      sessParams = GeneralSessionParameters 1000 $ ResultSet 10
      hostEndPoint = HostEndPoint "127.0.0.1" port (UserName userName) (Password password)
    in 
      SessionConfig 1 hostEndPoint sessParams "gl.csv" "accounts.txt"

createDefaultConfig :: SessionConfig 
createDefaultConfig = createConfig "loyakk_user1" "loyakk_password1" "8332"

endPoint :: HostEndPoint -> String 
endPoint (HostEndPoint hostName serviceName (UserName userName) (Password password)) = 
    "http://" <> userName <> ":" <> password <> "@" <> hostName <> ":" <> serviceName


data SessionConfig = SessionConfig {
  startRequestId :: Integer
  , hostEndPoint :: HostEndPoint
  , sessionParameters :: GeneralSessionParameters
  , outputFile :: FilePath
  , inputFile :: FilePath
} deriving(Show)

data SessionState = SessionState {
  nextRequestId :: RequestId
}

createDefaultState = 
    SessionState 
      (RequestId 1) 


instance Show SessionState where 
  show (SessionState r) = show "Session state : " <> (show r)

newtype Application a = Application {
  runA :: ReaderT SessionConfig (StateT SessionState IO) a
} deriving(Functor, Applicative, Monad, MonadIO, MonadReader SessionConfig, MonadState SessionState)

type SessionStatus a = Either String a

newtype SessionRequest = SessionRequest {_unReq :: Text} deriving (Show)
newtype SessionResponse = SessionResponse {_unRes :: Text} deriving(Show)

-- convenience functions 
readHeaderInformation :: Application (String, Wreq.Options, ResultSetSize)
readHeaderInformation = do 
  (SessionConfig sReqId hostEndPoint generalSessionParameters outputFile _) <- ask
  let fullyFormedEndPoint = endPoint hostEndPoint
  let opts = defaults
  let resultRows = resultSize generalSessionParameters
  return(fullyFormedEndPoint, opts, resultRows)  


{-- | Workflow for each account
  Retrieve
   * The address for the account.
   * All associated transaction summaries for the address.
   * Collect all the transaction details for each summary.
--}
processAccount :: AccountAddress -> Application [(AccountAddress, Address, Result RawTransaction)]
processAccount address = do 
  (SessionConfig sReqId hostEndPoint generalSessionParameters outputFile _) <- ask
  let fullyFormedEndPoint = endPoint hostEndPoint
  let opts = defaults
  let resultRows = resultSize generalSessionParameters
  (SessionState nReqId) <- State.get
  let req = getAccountAddress nReqId address
  response <- liftIO $ doPost opts fullyFormedEndPoint req
  debugMessage' $ Text.pack $ show response 
  State.modify (\s -> s {nextRequestId = 1 + nReqId})
  transactionSummaries <- processListByAddress address
  let transactionIds = truncateResultSet resultRows $ 
                            Prelude.map (\x -> _transactions x) $ 
                              Prelude.filter(\x -> _account x == address) $ 
                                Prelude.concat transactionSummaries
  
  r <- mapM transactionDetails $ truncateResultSet resultRows $ Prelude.concat transactionIds
  let r2 = Prelude.map (\(x,y) -> (address, Address x, y))r 
  return r2

truncateResultSet :: ResultSetSize ->  [a] -> [a]
truncateResultSet (ResultSet n) a = Prelude.take n a 
truncateResultSet AllRows a = a 

notEmptyAccountAddress :: TransactionSummary -> Bool
notEmptyAccountAddress summary = _account summary /= (AccountAddress "")


getListReceivedByAddress :: AccountAddress -> Integer -> Bool -> Bool -> RequestId -> Value 
getListReceivedByAddress accountAddress transactionCount includeEmpty includeWatchOnly (RequestId anId) = 
  let tranFrac = scientific transactionCount 0 in
  object[
    "jsonrpc" .= version
    , "id" .= anId
    , "method" .= ("listreceivedbyaddress" :: String)
    , "params" .= ([Number tranFrac, Bool includeEmpty, Bool includeWatchOnly] :: [Value])
  ]


type NextRequest = RequestId -> Value




doPost opts endPoint aRequest = do
  r <- asValue =<< postWith opts endPoint aRequest :: IO (Response Value)
  let respBody = r ^? responseBody . key "result"
  debugMessage $ Text.pack $ show aRequest
  if respBody ==  Nothing then 
    return $ Just $ (String $ Text.pack $ "Failed to return response : " <> (show aRequest))
  else 
    return respBody

errorMessage' :: Text -> Application ()
errorMessage' = liftIO . errorMessage

debugMessage' :: Text -> Application () 
debugMessage' = liftIO . debugMessage

infoMessage' :: Text -> Application ()
infoMessage' = liftIO . infoMessage 

transactionDetails :: Text -> Application (Text, Result RawTransaction)
transactionDetails anId = do 
  (SessionConfig _ hostEndPoint genParams outputFile _) <- ask
  (SessionState nReqId) <- State.get  
  let fullyFormedEndPoint = endPoint hostEndPoint
  let request = getRawTransaction nReqId anId
  debugMessage' $ Text.pack $ "Querying " <> (show anId)
  resp <- liftIO $ 
    handle (\e@(SomeException ecp) -> return . Just . String .pack $ "exception" <> (show e)) $ do 
      doPost defaults fullyFormedEndPoint request  
  State.modify (\s -> s {nextRequestId = 1 + nReqId})

  let r = case resp of 
            Just x -> fromJSON x
            Nothing -> Error $ show $ "unable to parse " <> anId <> " : " <> (Text.pack fullyFormedEndPoint)
  infoMessage' $ Text.pack $ show r 
  return (anId, r)

processListByAddress :: AccountAddress -> Application (Result [TransactionSummary])
processListByAddress accountAddress = do
  (SessionConfig _ hEndPoint gP' oFile _) <- ask
  (SessionState nReqId) <- State.get  
  let fullyFormedEndPoint = endPoint hEndPoint
  let request = getListReceivedByAddress accountAddress (transactionCount gP') includeEmpty includeWatchOnly nReqId
  response <- liftIO $ doPost defaults fullyFormedEndPoint request
  debugMessage' $ Text.pack $ show response
  State.modify (\s -> s {nextRequestId = 1 + nReqId})
  case response of 
    Just x -> return $ fromJSON x
    Nothing -> return $ Error "No transactions found"
  where
    includeEmpty = True
    includeWatchOnly = True




addresses :: [AccountAddress]
addresses = AccountAddress 
            <$> 
            ["16R9ffu5BPcKHoy3dvZLQeghqHgHecEVWF", "1Pc2hje5qW62oajtZqayfs83bcSn2EiGbJ"
            , "15fnGtAJrqzct2fyE4SpGFQBKMnWWmv5fs"
            , "1DZzpDWEacmFVsw8KTRB6uqVxf7nAYdzmW"
            , "1C6nxuqAmytbXR162nC3Xw2TMsYkQJNw9C"]


writeToFileWM aFile aMode aMessage = 
  bracket (openFile aFile aMode) (hClose) $ \h -> do 
    TextIO.hPutStrLn h aMessage

--writeToFile :: [[Text]] -> FilePath -> IO () 
writeToFile messages aFile aMode = 
  bracket (openFile aFile aMode) (hClose) $ \h -> do 
    TextIO.hPutStrLn h $ Text.unlines messages

readInputAccounts :: FilePath -> IO [AccountAddress]
readInputAccounts aFile = do
  bracket (openFile aFile ReadMode) (\_ -> return()) $ \h -> do 
    contents <- System.IO.hGetContents h 
    return $ AccountAddress . Text.pack <$> (Prelude.lines contents)

setupLogging = do 
  setup DEBUG
  addFileHandler "btc.debug.log" DEBUG
  addFileHandler "btc.info.log" INFO 


--generalLedgerApplication :: Traversable t => t AccountAddress -> IO [()]
generalLedgerApplication inputFileConfig = do 
  setupLogging
  config <- defaultFileLocation >>= readConfig 
  user <- btcUserName config
  passwordL <- btcPassword config
  port <- btcRpcPort config
  let 
    config1 = createConfig (Text.unpack user) (Text.unpack passwordL) (Text.unpack port)
    initialState = createDefaultState
  addressList <- readInputAccounts (inputFileConfig)
  transactionIds <- fst <$> (runStateT (runReaderT (runA $ traverse processAccount addressList) config1) initialState)
  messages <- return $ mapM (\y -> Prelude.map (\(x1, y1, z1) -> rawTransactionAsCSVR x1 y1 z1) y) transactionIds
  writeToFileWM (outputFile config1) WriteMode header -- Write the header, append messsages.
  mapM (\m -> writeToFile m (outputFile config1) AppendMode) messages
  where 
    header = "Account, Address, Confirmation, Time, BlockTime, Amount, Address"


{-- | 
  Search for all transactions for a block query.
  * Retrieve the block hash for a given block, to return all the transactions for the block.
  * For each block, fetch the raw transaction. 
  * Filter all transactions that have the address in vout.  
 2019  ~/bitcoin/src/bitcoin-cli getblockhash 504347 true

 2021  ~/bitcoin/src/bitcoin-cli getblock 00000000000000000002d265bc588c25eb0619b1ba10282a2881779bdaf71a45
 2022  ~/bitcoin/src/bitcoin-cli getblock 00000000000000000002d265bc588c25eb0619b1ba10282a2881779bdaf71a45 | less
 2023  ~/bitcoin/src/bitcoin-cli help | less
 2024  ~/bitcoin/src/bitcoin-cli getrawtransaction "21cce6eb5d6dbc86e9ff89dc24217fc4d2de1eae0037517d81225646c2f67fec"
 2025  ~/bitcoin/src/bitcoin-cli getrawtransaction "21cce6eb5d6dbc86e9ff89dc24217fc4d2de1eae0037517d81225646c2f67fec" true
 2026  ~/bitcoin/src/bitcoin-cli getrawtransaction "21cce6eb5d6dbc86e9ff89dc24217fc4d2de1eae0037517d81225646c2f67fec" true | less 
 2027  ~/bitcoin/src/bitcoin-cli getblock 00000000000000000002d265bc588c25eb0619b1ba10282a2881779bdaf71a45 | less
 2028  ~/bitcoin/src/bitcoin-cli getrawtransaction "21cce6eb5d6dbc86e9ff89dc24217fc4d2de1eae0037517d81225646c2f67fec" true | less 

--}


putStrLnA :: String -> Application () 
putStrLnA a = liftIO $ return ()


maybeToResult :: Maybe (Result a) -> Result a 
maybeToResult Nothing = Error "Nothing to do"
maybeToResult (Just a) = a


queryMatches :: BlockQuery -> RawTransaction -> Bool
queryMatches (BlockQuery (height, address)) aRawTransaction = True -- hasVOAddress address aRawTransaction

fetchBlockTransactions :: BlockQuery -> Block -> Application [RawTransaction]
fetchBlockTransactions query aBlock = do 
  let transactions = transactionList aBlock
  infoMessage' $ Text.pack $ "Querying transactions " <> (show $ Prelude.take 10 transactions)
  list <- mapM(\t -> transactionDetails t) $ Prelude.take 10 transactions  
  txList <- return $ mapM (\(x,y) -> y) list
  infoMessage' $ Text.pack $ "Tx list " <> (show txList)
  infoMessage' $ Text.pack $ "Tx list " <> (show list)    
  case txList of 
    Success txLs -> return $ Prelude.filter (\x -> queryMatches query x) txLs
    Error s -> return []

fetchBlockDetails :: BlockQuery -> BlockHash -> Application [RawTransaction]
fetchBlockDetails blockQuery aHash = do 
  (fullyFormedEndPoint, opts, resultRows) <- readHeaderInformation
  (SessionState nReqId) <- State.get
  let req = getBlock nReqId aHash 
  response <- liftIO $ doPost opts fullyFormedEndPoint req  
  --putStrLnA $ Prelude.take 1024 $ show response
  State.modify(\s -> s {nextRequestId = nReqId + 1})
  block <- return $ maybeToResult $ fromJSON <$> response
  case block of 
    Success bl -> 
      fetchBlockTransactions blockQuery bl
    Error s -> return $ []



fetchBlockHash :: BlockQuery -> Application [RawTransaction]
fetchBlockHash bQ@(BlockQuery blockQuery) = do 
  (fullyFormedEndPoint, opts, resultRows) <- readHeaderInformation
  (SessionState nReqId) <- State.get
  let req = getBlockHash nReqId $ fst blockQuery
  response <- liftIO $ doPost opts fullyFormedEndPoint req  
  -- putStrLnA $ show response
  State.modify(\s -> s {nextRequestId = nReqId + 1})
  resp <- return $ maybeToResult $ fromJSON <$> response
  case resp of 
    Success s -> fetchBlockDetails bQ s
    Error s1 -> return []




searchTransactions :: FilePath -> BlockQuery -> IO ([RawTransaction], SessionState)
searchTransactions inputFileConfig blockQuery = do 
  setupLogging
  config <- defaultFileLocation >>= readConfig 
  user <- btcUserName config
  passwordL <- btcPassword config
  port <- btcRpcPort config
  let 
    config1 = createConfig (Text.unpack user) (Text.unpack passwordL) (Text.unpack port)
    initialState = createDefaultState
  addressList <- readInputAccounts (inputFileConfig)
  result <- (runStateT (runReaderT (runA $ fetchBlockHash blockQuery) config1) initialState)
  infoMessage "--------------------------------"
  mapM (\x -> infoMessage $ Text.pack $ show x) result 
  return result
  -- Get the hash for a block.




getSingleTransaction :: FilePath -> BlockQuery -> IO ((Text, Result RawTransaction), SessionState)
getSingleTransaction inputFileConfig (BlockQuery(blockHeight, address))  = do 
--  setupLogging
  config <- defaultFileLocation >>= readConfig 
  user <- btcUserName config
  passwordL <- btcPassword config
  port <- btcRpcPort config
  let 
    config1 = createConfig (Text.unpack user) (Text.unpack passwordL) (Text.unpack port)
    initialState = createDefaultState
  addressList <- readInputAccounts (inputFileConfig)
  r <- (runStateT (runReaderT (runA $ transactionDetails "21cce6eb5d6dbc86e9ff89dc24217fc4d2de1eae0037517d81225646c2f67fec") config1) initialState)
  v <-return $  
      case (snd . fst $ r) of 
        Success r2 -> hasVOAddress (address) r2
        Error s -> False
  System.IO.putStrLn $ "Found address " <> (show v) <>  " " <> (show address)
  return r
  -- Get the hash for a block.


