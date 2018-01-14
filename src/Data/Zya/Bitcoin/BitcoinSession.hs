{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Zya.Bitcoin.BitcoinSession
(
  generalLedgerApplication
  , HostEndPoint(..)
  , GeneralSessionParameters(..)
  , addresses -- TODO remove this and read from a file.
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
import Data.Text.IO as TextIO
import Data.Zya.Bitcoin.Common 
import Data.Zya.Bitcoin.Common as BCommon
import Data.Zya.Bitcoin.JsonRPC hiding (getListReceivedByAddress, getAccountAddress, getRawTransaction)
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
  let req = getAccountAddress address nReqId
  response <- liftIO $ doPost opts fullyFormedEndPoint req
  debugMessage' $ Text.pack $ show response 
  State.modify (\s -> s {nextRequestId = 1 + nReqId})
  transactionSummaries <- processListByAddress address
  let transactionIds = truncateResultSet resultRows $ 
                            Prelude.map (\x -> _transactions x) $ 
                              Prelude.filter(\x -> _account x == address) $ 
                                Prelude.concat transactionSummaries
  debugMessage' $ Text.pack $ "transaction ids " <> (show $ Prelude.length transactionIds)
  r <- mapM transactionDetails $ truncateResultSet resultRows $ Prelude.concat transactionIds
  let r2 = Prelude.map (\(x,y) -> (address, Address x, y))r 
  return r2

truncateResultSet :: ResultSetSize ->  [a] -> [a]
truncateResultSet (ResultSet n) a = Prelude.take n a 
truncateResultSet AllRows a = a 

notEmptyAccountAddress :: TransactionSummary -> Bool
notEmptyAccountAddress summary = _account summary /= (AccountAddress "")


getRawTransaction :: Text -> RequestId -> Value 
getRawTransaction aTransactionId (RequestId anId) = 
  let 
    verbose = 1 -- 0 returns the hex bytestring.
  in 
    object[
      "jsonrpc" .= version
      , "id" .= anId
      , "method" .= ("getrawtransaction" :: String)
      , "params" .= [String aTransactionId, Number verbose]
    ]

getListReceivedByAddress :: AccountAddress -> Integer -> Bool -> Bool -> RequestId -> Value 
getListReceivedByAddress accountAddress transactionCount includeEmpty includeWatchOnly (RequestId anId) = 
  let tranFrac = scientific transactionCount 0 in
  object[
    "jsonrpc" .= version
    , "id" .= anId
    , "method" .= ("listreceivedbyaddress" :: String)
    , "params" .= ([Number tranFrac, Bool includeEmpty, Bool includeWatchOnly] :: [Value])
  ]


getAccountAddress :: AccountAddress -> RequestId -> Value
getAccountAddress (AccountAddress aString) (RequestId anId) = 
  let 
    params = object ["account" .= String aString] :: Value
  in 
  object 
  [
    "jsonrpc" .= version
    , "id" .= anId 
    , "method" .= ("getaccountaddress" :: Text)
    , "params" .= params
  ]


type NextRequest = RequestId -> Value




--doPost :: (WreqTypes.Postable a, Show a) => Wreq.Options -> String -> a -> IO (Maybe Value)
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

transactionDetails :: Text -> Application (Text, Result RawTransaction)
transactionDetails anId = do 
  (SessionConfig _ hostEndPoint genParams outputFile _) <- ask
  (SessionState nReqId) <- State.get  
  let fullyFormedEndPoint = endPoint hostEndPoint
  let request = getRawTransaction anId nReqId
  resp <- liftIO $ 
    handle (\e@(SomeException ecp) -> return . Just . String .pack . show $ e) $ do 
      doPost defaults fullyFormedEndPoint request
  errorMessage' $ Text.pack $ show resp
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


generalLedgerApplication' :: Traversable t => t AccountAddress -> IO (t [(AccountAddress, Address, Result RawTransaction)], SessionState)
generalLedgerApplication' addressList = do 
  setup DEBUG
  addFileHandler "btc.debug.log" DEBUG
  addFileHandler "btc.info.log" INFO
  let 
    config = createDefaultConfig
    initialState = createDefaultState
  runStateT (runReaderT (runA $ traverse processAccount addressList) config) initialState


--writeToFile :: [[Text]] -> FilePath -> IO () 
writeToFile messages aFile = 
  bracket (openFile aFile AppendMode) (hClose) $ \h -> do 
    TextIO.hPutStrLn h "Account, Address, Confirmation, Time, BlockTime, Amount, Address"
    TextIO.hPutStrLn h (Text.unlines messages)

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
  mapM (\m -> writeToFile m (outputFile config1)) $ Prelude.take 3 messages