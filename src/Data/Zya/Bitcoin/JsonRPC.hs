{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Bitcoin.JsonRPC
(
  getAccountAddress
  , getBlockHash
  , getBlock
  , getListReceivedByAddress
  , getRawTransaction
  , version 
  , getCreateNewAddress
  , getDumpPrivKey
) where 

import Data.Aeson
import Data.Scientific
import Data.Zya.Bitcoin.Block
import Data.Zya.Bitcoin.Common

import Data.Text as Text(pack, Text)
import Text.Printf(printf)

version :: String
version = "1.0"


getBlock :: RequestId -> BlockHash -> Value 
getBlock (RequestId anId) (BlockHash aHash) = 
  object[
    "jsonrpc" .= version
    , "id" .= anId
    , "method" .= ("getblock" :: Text)
    , "params" .= [String $ Text.pack $ printf "%x" (aHash :: Integer)]
    ]
getBlockHash :: RequestId -> BlockHeight -> Value
getBlockHash (RequestId anId) (BlockHeight aHeight) = 
  object[
    "jsonrpc" .= version
    , "id" .= anId
    , "method" .= ("getblockhash" :: Text)
    , "params" .= [Number $ scientific (toInteger aHeight) 0]
  ]

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


getCreateNewAddress :: RequestId -> Value 
getCreateNewAddress (RequestId anId) = 
  object
  [
    "jsonrpc" .= version
    , "id" .= anId
    , "method" .= ("getnewaddress" :: String)
    , "params" .= ([String ""])
  ]

getDumpPrivKey :: RequestId -> Address -> Value 
getDumpPrivKey (RequestId anId) (Address anAddress) = 
  let 
    params = object["address" .= anAddress]
  in
  object
  [
    "jsonrpc" .= version
    , "id" .= anId
    , "method" .= ("dumpprivkey" :: String)
    , "params" .= params
  ]