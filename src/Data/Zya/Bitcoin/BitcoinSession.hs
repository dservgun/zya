{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Zya.Bitcoin.BitcoinSession
(
  generalLedgerApplication
  , HostEndPoint(..)
  , GeneralSessionParameters(..)
  , addresses -- TODO remove this and read from a file.
  , searchTransactions
  , createAddresses
  , generateAddresses
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
import Network.Wreq.Types as WreqTypes
import qualified Network.Wreq.Session as WreqSession
import System.Log.Logger
import System.IO
import Data.Zya.Utils.Logger(setup, debugMessage, infoMessage, errorMessage, addFileHandler)
import Data.Zya.Bitcoin.Config



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


doPostWithS :: (Postable a, Show a) => Wreq.Options -> WreqSession.Session -> String -> a -> IO (Maybe Value)
doPostWithS opts session endPoint aRequest = do 
  r <- asValue =<< WreqSession.postWith opts session endPoint aRequest
  let respBody = r ^? responseBody . key "result"
  if respBody ==  Nothing then 
    return $ Just $ (String $ Text.pack $ "Failed to return response : " <> (show aRequest))
  else 
    return respBody

doPost :: (Postable a, Show a ) => Wreq.Options -> String -> a -> IO (Maybe Value)
doPost opts endPoint aRequest = do
  r <- asValue =<< postWith opts endPoint aRequest :: IO (Response Value)
  let respBody = r ^? responseBody . key "result"
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




toTransaction :: Maybe Value -> Result RawTransaction
toTransaction Nothing = Error $ "Unable to parse" 
toTransaction (Just x) = fromJSON x  

transactionDetailsRequest :: SessionState -> SessionConfig -> Text -> Value
transactionDetailsRequest (SessionState nReqId) 
              config@(SessionConfig _ hostEndPoint genParams outputFile _ ) anId =
  let 
    fullyFormedEndPoint = endPoint hostEndPoint
  in 
    getRawTransaction nReqId anId

transactionDetailsWithSession :: WreqSession.Session -> Text -> Application(Text, Result RawTransaction)
transactionDetailsWithSession aSession anId = do 
  c@(SessionConfig _ hostEndPoint genParams outputFile _) <- ask
  s@(SessionState nReqId) <- State.get  
  let fullyFormedEndPoint = endPoint hostEndPoint
  let request = transactionDetailsRequest s c anId
  resp <- liftIO $ doPostWithS defaults aSession fullyFormedEndPoint request
  State.modify(\s -> s {nextRequestId = nReqId + 1})
  debugMessage' (Text.pack $ show resp)
  return $ (anId, toTransaction resp)

transactionDetailsInBulk :: [Text] -> Application[Result RawTransaction]
transactionDetailsInBulk requestIds = do 
  c@(SessionConfig _ hostEndPoint genParams outputFile _) <- ask
  s@(SessionState nReqId) <- State.get  
  let fullyFormedEndPoint = endPoint hostEndPoint
  requestObjects <- 
        mapM (\requestId -> do 
                  s'@(SessionState nReqId') <- State.get
                  let r = transactionDetailsRequest s c requestId
                  State.modify(\s -> s {nextRequestId = nReqId' + 1})
                  return (r, requestId)
                  ) requestIds
  resp <- 
      liftIO $ WreqSession.withSession $ \session ->  
                mapM (\(requestObj, requestId) -> do
                            debugMessage $ Text.pack $ show requestObj
                            handle(\e@(SomeException exc) -> 
                                          (debugMessage $ requestId <> ":" <> (Text.pack $ show e)) >> return Nothing) $  
                                doPostWithS 
                                defaults 
                                session 
                                fullyFormedEndPoint 
                                requestObj) requestObjects

  return $ fmap toTransaction resp

transactionDetails :: Text -> Application (Text, Result RawTransaction)
transactionDetails anId = do 
  (SessionConfig _ hostEndPoint genParams outputFile _) <- ask
  (SessionState nReqId) <- State.get  
  let fullyFormedEndPoint = endPoint hostEndPoint
  let request = getRawTransaction nReqId anId
  resp <- liftIO $ 
    handle (\e@(SomeException ecp) -> return . Just . String .pack $ "exception" <> (show e)) $ do 
      doPost defaults fullyFormedEndPoint request  
  State.modify (\s -> s {nextRequestId = 1 + nReqId})

  let r = case resp of 
            Just x -> fromJSON x
            Nothing -> Error $ show $ "unable to parse " <> anId <> " : " <> (Text.pack fullyFormedEndPoint)
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
  lines <- readInputLines aFile
  return $ Prelude.map (AccountAddress . Text.pack)lines


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


maybeToResult :: Maybe (Result a) -> Result a 
maybeToResult Nothing = Error "Nothing to do"
maybeToResult (Just a) = a

isSuccessR :: Result RawTransaction -> Bool
isSuccessR (Success _) = True
isSuccessR _ = False

successR :: [Result RawTransaction] -> [RawTransaction] 
successR aList = 
    Prelude.map(\x@(Success y) -> y) $ Prelude.filter(isSuccessR) aList

queryMatches :: BlockQuery -> RawTransaction -> Bool
queryMatches (BlockQuery (height, addresses)) aRawTransaction = 
    Prelude.foldr 
      (\ele acc -> acc || ele) 
      False 
      $ Prelude.map(\x -> hasVOAddress x aRawTransaction) addresses

queryMatchesR :: BlockQuery -> Result RawTransaction -> Bool 
queryMatchesR q (Success t) = queryMatches q t 
queryMatchesR q _ = False

queryMatchesM :: BlockQuery -> RawTransaction -> Application Bool 
queryMatchesM = \q t -> do 
  result <- return $ queryMatches q t 
  debugMessage' $ Text.pack $ "Querying " <> (show q) <> " : " <> (show t)
  return result 

printDebugMessages :: (Show a) => [a] -> IO ()
printDebugMessages aList = mapM_ (\x -> debugMessage $ Text.pack $ show x) aList 

fetchBlockTransactionsWithSession :: BlockQuery -> Block -> Application [RawTransaction]
fetchBlockTransactionsWithSession query aBlock = do
  let transactions = transactionList aBlock 
  list <- transactionDetailsInBulk transactions
  list2 <- return $ Prelude.filter(queryMatchesR query) list
  liftIO $ printDebugMessages list
  liftIO $ printDebugMessages $ successR list2
  return $ successR list2
  

fetchBlockTransactions :: BlockQuery -> Block -> Application [RawTransaction]
fetchBlockTransactions query aBlock = do 
  let transactions = transactionList aBlock
  --session <- liftIO $ WreqSession.newSession
  list <- mapM (\t -> transactionDetails t) transactions
  list2 <- return $ Prelude.map (\(t, q) -> q) list
  list3 <- return $ Prelude.filter(queryMatchesR query) list2
  debugMessage' $ Text.pack $ " after map " <> (show list3)
  return $ successR list3


createAddress :: Application(Result Address, Result Address)
createAddress = do 
  (fullyFormedEndPoint, opts, resultRows) <- readHeaderInformation 
  (SessionState nReqId) <- State.get 
  let req = getCreateNewAddress nReqId 
  response <- liftIO $ doPost opts fullyFormedEndPoint req
  let returnedAddress = maybeToResult $ fromJSON <$> response
  State.modify(\s -> s {nextRequestId = nReqId + 1})
  (SessionState nReqId) <- State.get
  responseAddress <-
      case returnedAddress of 
        Success address -> do 
          let addressDetails = getDumpPrivKey nReqId address
          dumpPrivKey <- liftIO $ doPost opts fullyFormedEndPoint addressDetails
          return $ maybeToResult $ fromJSON <$> dumpPrivKey
  return (returnedAddress, responseAddress) 
createAddressesM :: Int -> Application[(Result Address, Result Address)]
createAddressesM n = replicateM n createAddress 

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
    Success bl -> fetchBlockTransactionsWithSession blockQuery bl
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



headerString = "Account, Address, Confirmation, Time, BlockTime, Amount, Address"


searchTransactions :: FilePath -> BlockQuery -> IO ([RawTransaction], SessionState)
searchTransactions inputFileConfig blockQuery = do 
  config <- defaultFileLocation >>= readConfig 
  user <- btcUserName config
  passwordL <- btcPassword config
  port <- btcRpcPort config
  infoMessage $ Text.pack $ "Query " <> show blockQuery
  let 
    config1 = createConfig (Text.unpack user) (Text.unpack passwordL) (Text.unpack port)
    initialState = createDefaultState
  addressList <- readInputAccounts (inputFileConfig)
  result <- (runStateT (runReaderT (runA $ fetchBlockHash blockQuery) config1) initialState)
  infoMessage "--------------------------------"
  infoMessage $ Text.pack $ show result
  csvResults <- 
    return $ 
        (Prelude.map(\s -> rawTransactionAsCSV (AccountAddress "t")(Address "a") s) $ fst result)
  infoMessage $ Text.pack $ show csvResults
  writeToFileWM (outputFile config1) AppendMode header -- Write the header, append messsages.
  writeToFile csvResults (outputFile config1) AppendMode
  return result
  where 
    header = "Account, Address, Confirmation, Time, BlockTime, Amount, Address"



-- Create n number of addresses with public and private key
createAddresses :: Int -> IO ([(Result Address, Result Address)], SessionState)
createAddresses n = do 
  config1 <- defaultFileLocation >>= readConfig 
  user <- btcUserName config1
  passwordL <- btcPassword config1
  port <- btcRpcPort config1 
  let config = 
        createConfig (Text.unpack user) (Text.unpack passwordL) (Text.unpack port)
      initialState = createDefaultState
  runStateT (runReaderT (runA $ createAddressesM n) config) initialState



generateAddresses n = do 
  (addressesL, state) <- createAddresses n 
  TextIO.putStrLn $ 
      intercalate "\n" $ 
        Prelude.map (\(Success (Address x), Success (Address y)) -> 
          (Text.pack $ show x) <> "->" <>  (Text.pack $ show y)) addressesL
