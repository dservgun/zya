{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Zya.Bitcoin.BitcoinSession
(
  Application
  , SessionStatus
  , SessionConfig(..)
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
import Data.Zya.Bitcoin.Common 
import Data.Zya.Bitcoin.Common as BCommon
import Data.Zya.Bitcoin.JsonRPC hiding (getListReceivedByAddress, getAccountAddress, getRawTransaction)
import Data.Zya.Bitcoin.RawTransaction as RawTransaction
import Data.Zya.Bitcoin.Transaction as Transaction
import Data.Zya.Utils.IPC
import Network.HTTP.Client hiding(responseBody)
import Network.Socket
import Network.Wreq


-- Some constants such as transaction count when 
-- accessing the wallet. This should
-- more become a map rather than single instance,
-- each request type could have a set of defaults
-- that we would like to know about.
data GeneralSessionParameters = GeneralSessionParameters {
  transactionCount :: Integer
} deriving(Show)
data HostEndPoint = HostEndPoint {
  hostName :: HostName
  , serviceName :: ServiceName
  , userName :: UserName
  , password :: Password 
} deriving(Show)


createDefaultConfig :: SessionConfig 
createDefaultConfig = 
    let 
      -- someDefaults = ("127.0.0.1", "8332")
      -- userName = UserName "loyakk_user1"
      -- password = Password "loyakk_password1"
      sessParams = GeneralSessionParameters 1
      hostEndPoint = HostEndPoint "127.0.0.1" "8332" (UserName "loyakk_user1") (Password "loyakk_password1")
    in 
      SessionConfig 1 hostEndPoint sessParams "abc"

endPoint :: HostEndPoint -> String 
endPoint (HostEndPoint hostName serviceName (UserName userName) (Password password)) = 
    "http://" <> userName <> ":" <> password <> "@" <> hostName <> ":" <> serviceName


data SessionConfig = SessionConfig {
  startRequestId :: Integer
  , hostEndPoint :: HostEndPoint
  , sessionParameters :: GeneralSessionParameters
  , outputFile :: FilePath
} deriving(Show)

data SessionState = SessionState {
  nextRequestId :: RequestId
}

createDefaultState = 
    SessionState 
      (RequestId "1") 


instance Show SessionState where 
  show (SessionState r) = show "Session state : " <> (show r)

newtype Application a = Application {
  runA :: ReaderT SessionConfig (StateT SessionState IO) a
} deriving(Functor, Applicative, Monad, MonadIO, MonadReader SessionConfig, MonadState SessionState)

type SessionStatus a = Either String a

newtype SessionRequest = SessionRequest {_unReq :: Text} deriving (Show)
newtype SessionResponse = SessionResponse {_unRes :: Text} deriving(Show)

-- Workflow 
-- for each account
-- get address for the account
-- get 


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

incrementRequestId :: RequestId -> RequestId 
incrementRequestId (RequestId anId) = RequestId $ anId <> (Text.pack $ show 1) -- Some counter.




doPost opts endPoint aRequest = do
  r <- asValue =<< postWith opts endPoint aRequest :: IO (Response Value)
  let respBody = r ^? responseBody . key "result"
  putStrLn $ (show respBody)
  if respBody ==  Nothing then 
    return $ Just $ (String $ Text.pack $ "Failed to return response : " <> (show aRequest))
  else 
    return respBody


transactionDetails :: Text -> Application (Maybe (Result RawTransaction))
transactionDetails anId = do 
  (SessionConfig _ hostEndPoint genParams outputFile) <- ask
  (SessionState nReqId) <- State.get  
  let fullyFormedEndPoint = endPoint hostEndPoint
  let request = getRawTransaction anId nReqId
  resp <- liftIO $ doPost defaults fullyFormedEndPoint request
  --System.IO.putStrLn $ show resp
  return $ fromJSON <$> resp


processListByAddress ::  AccountAddress -> Application (Maybe (Result TransactionSummary))
processListByAddress accountAddress = do
  (SessionConfig _ hostEndPoint genParams outputFile) <- ask
  (SessionState nReqId) <- State.get  
  let fullyFormedEndPoint = endPoint hostEndPoint
  let request = getListReceivedByAddress accountAddress (transactionCount genParams) includeEmpty includeWatchOnly nReqId
  response <- liftIO $ doPost defaults fullyFormedEndPoint request
  return $ fromJSON <$> response
  where
    includeEmpty = True
    includeWatchOnly = True


getTexts :: Result(Maybe [Text]) -> [Text]
getTexts aResultSet = 
  case aResultSet of 
      Success y -> 
        case y of 
            Just z -> z
            _ -> []
      _ -> []

processAccount :: AccountAddress -> Application [Maybe (Result RawTransaction)]
processAccount address = do 
  (SessionConfig sReqId hostEndPoint generalSessionParameters outputFile) <- ask
  let fullyFormedEndPoint = endPoint hostEndPoint
  let opts = defaults
  (SessionState nReqId) <- State.get
  let req = getAccountAddress address nReqId
  response <- liftIO $ doPost opts fullyFormedEndPoint req
  State.modify (\s -> s {nextRequestId = incrementRequestId nReqId})
  transactionSummaries <- processListByAddress address
  let transactionIds = mapM (\x -> fmap _transactions x) transactionSummaries
  mapM transactionDetails $ getTexts transactionIds


--generalLedgerForAddress :: Address -> IO (SessionStatus AccountAddress, SessionState)
generalLedgerForAddress anAddress = do 
  let 
    config = createDefaultConfig
    state = createDefaultState
  runStateT(runReaderT (runA (processAccount anAddress)) config) state



addresses :: [AccountAddress]
addresses = AccountAddress 
            <$> 
            ["16R9ffu5BPcKHoy3dvZLQeghqHgHecEVWF", "1Pc2hje5qW62oajtZqayfs83bcSn2EiGbJ"
            , "15fnGtAJrqzct2fyE4SpGFQBKMnWWmv5fs"
            , "1DZzpDWEacmFVsw8KTRB6uqVxf7nAYdzmW"
            , "1C6nxuqAmytbXR162nC3Xw2TMsYkQJNw9C"]

f1 = generalLedgerForAddress $ AccountAddress "16R9ffu5BPcKHoy3dvZLQeghqHgHecEVWF"

{-unwrapSendTransactionIO :: Socket -> String -> TransactionRequest -> IO(Maybe Value, SessionState) 
unwrapSendTransactionIO socket accountAddress transactionRequest = do 
  let 
    config = SessionConfig 1 socket (read accountAddress) 0 0
    state = SessionState 0 Nothing 
  runStateT (runReaderT (runA (sendTransactionInSession transactionRequest)) config) state
-}