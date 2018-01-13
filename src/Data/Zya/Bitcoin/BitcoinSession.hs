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
  , resultSize :: Int
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
      sessParams = GeneralSessionParameters 6 10
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
processAccount :: AccountAddress -> Application [Result RawTransaction]
processAccount address = do 
  (SessionConfig sReqId hostEndPoint generalSessionParameters outputFile) <- ask
  let fullyFormedEndPoint = endPoint hostEndPoint
  let opts = defaults
  let resultRows = resultSize generalSessionParameters
  (SessionState nReqId) <- State.get
  let req = getAccountAddress address nReqId
  response <- liftIO $ doPost opts fullyFormedEndPoint req
  State.modify (\s -> s {nextRequestId = incrementRequestId nReqId})
  transactionSummaries <- processListByAddress address
  let transactionIds = Prelude.take resultRows $ Prelude.map (\x -> _transactions x) $ Prelude.concat transactionSummaries
  liftIO $ putStrLn $ "transaction ids " <> (show $ Prelude.length transactionIds)
  r <- mapM transactionDetails $ Prelude.concat transactionIds
  return r


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
incrementRequestId input = input + 1




doPost opts endPoint aRequest = do
  r <- asValue =<< postWith opts endPoint aRequest :: IO (Response Value)
  let respBody = r ^? responseBody . key "result"
  putStrLn $ show aRequest
  if respBody ==  Nothing then 
    return $ Just $ (String $ Text.pack $ "Failed to return response : " <> (show aRequest))
  else 
    return respBody


transactionDetails :: Text -> Application (Result RawTransaction)
transactionDetails anId = do 
  (SessionConfig _ hostEndPoint genParams outputFile) <- ask
  (SessionState nReqId) <- State.get  
  let fullyFormedEndPoint = endPoint hostEndPoint
  let request = getRawTransaction anId nReqId
  resp <- liftIO $ doPost defaults fullyFormedEndPoint request
  State.modify (\s -> s {nextRequestId = incrementRequestId nReqId})
  --liftIO $ putStrLn $ show resp
  return $ 
    case resp of 
      Just x -> fromJSON x 
      Nothing -> Error $ show $ "unable to parse " <> anId <> " : " <> (Text.pack fullyFormedEndPoint)


processListByAddress ::  AccountAddress -> Application (Result [TransactionSummary])
processListByAddress accountAddress = do
  (SessionConfig _ hostEndPoint genParams outputFile) <- ask
  (SessionState nReqId) <- State.get  
  let fullyFormedEndPoint = endPoint hostEndPoint
  let request = getListReceivedByAddress accountAddress (transactionCount genParams) includeEmpty includeWatchOnly nReqId
  response <- liftIO $ doPost defaults fullyFormedEndPoint request
  liftIO $ putStrLn $ Prelude.take 1024 $ show response
  State.modify (\s -> s {nextRequestId = incrementRequestId nReqId})
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

f1 addressList = do
  mapM processAccount addressList

f2 addressList = do 
  let 
    config = createDefaultConfig
    state = createDefaultState
  runStateT (runReaderT (runA $ f1 addressList) config) state
{-unwrapSendTransactionIO :: Socket -> String -> TransactionRequest -> IO(Maybe Value, SessionState) 
unwrapSendTransactionIO socket accountAddress transactionRequest = do 
  let 
    config = SessionConfig 1 socket (read accountAddress) 0 0
    state = SessionState 0 Nothing 
  runStateT (runReaderT (runA (sendTransactionInSession transactionRequest)) config) state
-}