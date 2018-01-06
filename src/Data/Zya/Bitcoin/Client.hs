{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Bitcoin.Client where

import Data.Aeson
import Data.Monoid
import Data.Map as Map
import Data.Scientific
import Data.Text
import Data.Zya.Utils.IPC
import Data.Zya.Utils.JsonRPC
import Network.Socket
import System.IO
import Control.Exception

import Control.Exception as E
import Control.Lens hiding ((.=))
import Network.HTTP.Client hiding(responseBody)
import Network.Wreq
import Data.ByteString
import Data.ByteString.Lazy
import Data.Text as T
import Data.Text.Encoding (decodeUtf8)

version :: String
version = "1.0"
newtype RequestId = RequestId {id :: Text} deriving(Show)
newtype AccountAddress = AccountAddress {_accountAddress :: Text} deriving(Show)


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

getJSORRpcResponse hostName serviceName (UserName userName) (Password password) aRequest= do 
  let endPoint = "http://" <> userName <> ":" <> password <> "@" <> hostName <> ":" <> serviceName
  let u = toStrict $ encode $ T.pack $ userName 
  let p = toStrict $ encode $ T.pack $ password
  let opts = defaults
  r <- asJSON =<< postWith opts endPoint aRequest :: IO Resp
  return $ r ^. responseBody



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
loadAddresses :: (String, String) -> UserName -> Password -> [AccountAddress] -> IO [Map String Value]
loadAddresses defaults userName password addresses =  
  mapM
      (\addr -> 
        getJSORRpcResponse 
          (fst someDefaults) 
          (snd someDefaults) 
          userName
          password
          $ getAccountAddress (RequestId "test") addr) addresses



testDefaults = 
  let 
    userName = UserName "loyakk_user1"
    password = Password "loyakk_password1"
  in
  handle (\a@(SomeException e) ->  (System.IO.putStrLn $ "Exception ***" <> show a) >> return (Map.empty)) $ do 
    _ <- 
      loadAddresses someDefaults 
        (UserName "loyakk_user1")
        (Password "loyakk_password1") 
        addresses
    getJSORRpcResponse 
      (fst someDefaults) 
      (snd someDefaults) 
      userName
      password
      $ getListReceivedByAddress (RequestId "1") 6 True True



