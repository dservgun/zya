{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Ethereum.Internal.Types.RPCRequest where

import Data.Aeson
import Text.Printf
import Data.Monoid
import Data.Text
data RPCRequest = RPCRequest {
  rpcVersion :: String
  , method :: String
  , id :: Int 
  , params :: Maybe [String]
}

type Param = Value -- Need to confirm this
type Method = String 

methodPrefix :: String 
methodPrefix = "web3"


data DefaultParameters = DefaultParameters {
  defaultRpcVersion :: String
  , defaultMethodPrefix :: String
} deriving (Show) 

defaultParameters = DefaultParameters "2.0" "web3"
defaultNetMethodParameters = DefaultParameters "2.0" "net"
defaultEthMethodParameters = DefaultParameters "2.0" "eth"
createRPCRequest :: DefaultParameters -> Method -> Int -> [Param] -> Value
createRPCRequest defaultParams method requestId params = 
  object[
      "jsonrpc" .= rpcVersionL 
      , "method" .= methodWithPrefix
      , "id" .= requestId 
      , "params" .= params
      ]

  where 
    methodWithPrefix = (defaultMethodPrefix defaultParams) <> "_" <> method
    rpcVersionL = defaultRpcVersion defaultParams


createRPCRequestWithDefaults = createRPCRequest defaultParameters 


----- Eth client JSON Rpc methods --- 
clientVersion anId = 
    createRPCRequestWithDefaults "clientVersion" anId []
net_peerCount anId = 
    createRPCRequest defaultNetMethodParameters "peerCount" anId []
net_listening anId = 
    createRPCRequest defaultNetMethodParameters "listening" anId []
sha3 aParam anId = 
  createRPCRequestWithDefaults "sha3" anId [aParam]
net_version anId = 
  createRPCRequest 
    defaultNetMethodParameters 
    "version" anId []
eth_protocolVersion anId = 
  createRPCRequest
    defaultEthMethodParameters 
    "protocolVersion" anId []

eth_syncing anId = 
  createRPCRequest
    defaultEthMethodParameters
    "syncing" anId
-- Fix this
eth_coinbase anId = 
  createRPCRequest
    defaultEthMethodParameters
    "coinbase" anId    

-- This call is not working.
eth_compilers anId = 
  createRPCRequest 
    defaultEthMethodParameters
      "get_compilers"
      anId

data BlockQuantity = Earliest | Latest | Pending | BlockId Integer deriving (Show)

-- Retrieve all the details.
eth_getBlockByNumber :: Int -> BlockQuantity -> Bool -> Value 
eth_getBlockByNumber anId blockQuantity details = 
  case blockQuantity of
    Earliest -> 
      createRPCRequest defaultEthMethodParameters "getBlockByNumber" anId [String "earliest", Bool details]
    Latest -> 
      createRPCRequest defaultEthMethodParameters "getBlockByNumber" anId [String "latest", Bool details]
    Pending -> 
      createRPCRequest defaultEthMethodParameters "gethBlockByNumber" anId [String "pending" , Bool details]
    BlockId anInteger -> 
      createRPCRequest defaultEthMethodParameters "getBlockByNumber" anId $ 
          [String (blockId anInteger), Bool details]
      where
        blockId :: Integer -> Text
        blockId anId = pack (Text.Printf.printf "0x%x" anId)
