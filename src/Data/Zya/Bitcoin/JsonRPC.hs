{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Bitcoin.JsonRPC
(
  getRawTransaction
  , getAccountAddress
  , getListReceivedByAddress
  , version 
) where 

import Data.Aeson
import Data.Scientific
import Data.Zya.Bitcoin.Common
import Data.Text

version :: String
version = "1.0"

getRawTransaction :: RequestId -> Text -> Value
getRawTransaction (RequestId anId) aTransaction = 
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
