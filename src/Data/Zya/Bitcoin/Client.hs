{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Bitcoin.Client where


import Control.Exception
import Control.Exception as E
import Control.Monad
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens(key, nth)
import Data.ByteString
import Data.ByteString.Lazy
import Data.Map as Map
import Data.Monoid
import Data.Scientific
import Data.Text as T
import Data.Text.IO as TextIO
import Data.Text.Encoding (decodeUtf8)
import Data.Zya.Bitcoin.Transaction as Transaction
import Data.Zya.Utils.IPC
import Data.Zya.Utils.JsonRPC
import Network.HTTP.Client hiding(responseBody)
import Network.Socket
import Network.Wreq
import System.IO

version :: String
version = "1.0"



getTransactionDetail :: RequestId -> Text -> Value
getTransactionDetail (RequestId anId) aTransaction = 
  object [
    "jsonrpc" .= version
    , "id" .= anId 
    , "method" .= ("getrawtransaction" :: Text)
    , "params" .= [String aTransaction, Number 1]
  ]
getAccountAddress :: RequestId -> AccountAddress -> Value
getAccountAddress (RequestId anId) (AccountAddress aString) = 
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


getListReceivedByAddress :: RequestId -> Integer -> Bool -> Bool -> Value 
getListReceivedByAddress (RequestId anId) transactionCount includeEmpty includeWatchOnly = 
  let tranFrac = scientific transactionCount 0 in
  object[
    "jsonrpc" .= version
    , "id" .= anId
    , "method" .= ("listreceivedbyaddress" :: String)
    , "params" .= ([Number tranFrac, Bool includeEmpty, Bool includeWatchOnly] :: [Value])
  ]


{-
  curl --user user --data-binary '{"jsonrpc": "1.0", "id":"curltest", "method": "getinfo", "params": [] }' 
    -H 'content-type: text/plain;' http://127.0.0.1:8332/
-}


type Resp = Response (Map String Value)
newtype UserName = UserName {_uName :: String} deriving(Show) 
newtype Password = Password {_uPassword :: String} deriving(Show)

getJSONRpcResponse hostName serviceName (UserName userName) (Password password) aRequest= do 
  let endPoint = "http://" <> userName <> ":" <> password <> "@" <> hostName <> ":" <> serviceName
  let u = toStrict $ encode $ T.pack $ userName 
  let p = toStrict $ encode $ T.pack $ password
  let opts = defaults
  r <- asValue =<< postWith opts endPoint aRequest :: IO (Response Value)
  let respBody = r ^? responseBody . key "result"
  return respBody



someDefaults :: (String, String)
someDefaults = ("127.0.0.1", "8332")

addresses :: [AccountAddress]
addresses = AccountAddress 
            <$> 
            ["16R9ffu5BPcKHoy3dvZLQeghqHgHecEVWF", "1Pc2hje5qW62oajtZqayfs83bcSn2EiGbJ"
            , "15fnGtAJrqzct2fyE4SpGFQBKMnWWmv5fs"
            , "1DZzpDWEacmFVsw8KTRB6uqVxf7nAYdzmW"
            , "1C6nxuqAmytbXR162nC3Xw2TMsYkQJNw9C"]

-- Load all addresses in the client
loadAddresses :: (String, String) -> UserName -> Password -> [AccountAddress] -> IO [Maybe Value]
loadAddresses defaults userName password addresses =  
  mapM
      (\addr -> 
        getJSONRpcResponse 
          (fst someDefaults) 
          (snd someDefaults) 
          userName
          password
          $ getAccountAddress (RequestId "test") addr) addresses


transactionSummaries :: IO (Maybe (Result [TransactionSummary]))
transactionSummaries = 
  let 
    userName = UserName "loyakk_user1"
    password = Password "loyakk_password1"
  in
  handle (\a@(SomeException e) -> return $ Just $ Data.Aeson.Error (show a)) $ do 
    _ <- 
      loadAddresses someDefaults 
        (UserName "loyakk_user1")
        (Password "loyakk_password1") 
        addresses
    -- transaction summaries.
    resp <- 
        getJSONRpcResponse 
          (fst someDefaults) 
          (snd someDefaults) 
          userName
          password
          $ getListReceivedByAddress (RequestId "1") 6 True True
    let transactionSummaries = fromJSON <$> resp
    return $ transactionSummaries

transactionDetails :: UserName -> Password -> Text -> IO (Maybe(Result Transaction)) 
transactionDetails userName password anId = do 
  resp <- getJSONRpcResponse 
            (fst someDefaults)
            (snd someDefaults)
            userName 
            password 
            $ getTransactionDetail (RequestId "1") (anId)
  System.IO.putStrLn $ show resp
  return $ fromJSON <$> resp

-- scary types.
transactionIds' = (fmap . fmap . fmap) (Prelude.map (\x -> (_account x, _address x, _transactions x))) transactionSummaries

transactionIds = transactionIds'


transactionDetailsWithDefaults = 
    transactionDetails (UserName "loyakk_user1") (Password "loyakk_password1")

formatCSV :: (AccountAddress, Transaction.Address, Transaction) -> String
formatCSV (AccountAddress account, Transaction.Address address, 
          Transaction amount conf blockH blockT txid _ time timeR _ _ _
          ) = 
      (show account) <> ", " 
      <> (show address) <> "," 
      <> (show amount) <> ","
      <> (show conf) <> ","
      <> (show blockH) <> ","
      <> (show blockT) <> "," 
      <> (show txid) <> "," 
      <> (show time) <> ","
      <> (show timeR)
format :: (AccountAddress, Transaction.Address, [Maybe (Result Transaction)]) -> [Text]
format (account, address, transactions) = 
  Prelude.map(\l -> case l of 
                      Just (Success x) -> T.pack $ formatCSV(account, address, x)) transactions

-- All transaction details with account information
f2 = do 
  x <- transactionIds
  case x of 
    Just y -> do      
      case y of 
        Success aList -> do 
          z1 <- mapM (\a@(x, addr, y) -> do 
                z <- mapM transactionDetailsWithDefaults y 
                return $ format (x, addr, z)) aList
          return $ Prelude.concat z1


writeToFile aFile = do 
  bracket (openFile aFile WriteMode) (hClose) $ \h -> do 
    TextIO.hPutStrLn h "Account, Address, Amount, Confirmation, Block Hash, Block Time, Transaction Id, Time, Time Received"
    f <- f2
    TextIO.hPutStrLn h (T.unlines f)
