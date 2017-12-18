{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Ethereum.Internal.Types.RPCRequest where

import Data.Aeson
import Text.Printf
import Data.Monoid
import Data.Text

newtype Request = Request {_unRequest :: Int} deriving(Show)
newtype StoragePosition = StoragePosition {_unSP :: Int} deriving(Show)

type Param = Value -- Need to confirm this
type Method = String 

methodPrefix :: String 
methodPrefix = "web3"


newtype Address = Address {_unAddress :: String} 
newtype Hash = Hash {_unHash :: String}


data DefaultParameters = DefaultParameters {
  defaultRpcVersion :: String
  , defaultMethodPrefix :: String
} deriving (Show) 

type Quantity = Integer

data BlockQuantity = Earliest | Latest | Pending | BlockId Integer

instance Show BlockQuantity where 
  show a = 
    case a of
      Earliest -> "earliest" 
      Latest -> "latest"
      Pending -> "pending" 
      BlockId anId -> printf "%d" anId

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




eth_mining :: Int -> Value
eth_mining anId = 
  createRPCRequest defaultEthMethodParameters "mining" anId []

eth_hashrate :: Int -> Value
eth_hashrate anId = 
  createRPCRequest defaultEthMethodParameters "hashrate" anId []

eth_gasPrice :: Int -> Value
eth_gasPrice anId = 
  createRPCRequest defaultEthMethodParameters "gasPrice" anId []

eth_accounts :: Int -> Value 
eth_accounts anId = 
  createRPCRequest defaultEthMethodParameters "accounts" anId []

eth_blockNumber :: Int -> Value
eth_blockNumber anId = 
  createRPCRequest defaultEthMethodParameters "blockNumbers" anId []

eth_getBalance :: Int -> Address -> BlockQuantity -> Value
eth_getBalance anId (Address anAddress) defaultQuantity = 
  createRPCRequest 
    defaultEthMethodParameters
    "getBalance"
    anId
    [String . pack . convertToAddress $ anAddress
    , String . pack . show $ defaultQuantity]


eth_getStorageAt :: Int -> Address -> StoragePosition -> BlockQuantity -> Value 
eth_getStorageAt anId (Address anAddress) (StoragePosition aPosition) defaultQuantity = 
  createRPCRequest defaultEthMethodParameters 
    "getStorageAt"
    anId 
    [String . pack . convertToAddress $ anAddress
    , String . pack . convertIntToHex $ aPosition
    , String . pack . show $ defaultQuantity
    ]

eth_getTransactionCount :: Int -> Address -> BlockQuantity -> Value 
eth_getTransactionCount anId (Address anAddress) (defaultQuantity) = 
  createRPCRequest defaultEthMethodParameters
  "getTransactionCount" 
  anId 
  [
    String . pack . convertToAddress $ anAddress
    , String . pack . show $ defaultQuantity
  ]

eth_getBlockTransactionCountByHash :: Int -> Hash -> BlockQuantity -> Value
eth_getBlockTransactionCountByHash anId (Hash aHash) defaultQuantity = 
  createRPCRequest defaultEthMethodParameters
    "getBlockTransactionCountByHash"
    anId
    [
      String . pack $ aHash
    ]

eth_getBlockTransactionCountByNumber :: Int -> Int -> BlockQuantity -> Value 
eth_getBlockTransactionCountByNumber anId transactionId defaultQuantity = 
  createRPCRequest defaultEthMethodParameters
    "getBlockTransactionCountByNumber" 
    anId
    [String . pack . convertIntToHex $ transactionId]

eth_getUncleCountByBlockHash :: Int -> Hash -> Value 
eth_getUncleCountByBlockHash anId (Hash aHash) = 
  createRPCRequest 
    defaultEthMethodParameters
    "getUncleCountByBlockHash"
    anId 
    [String . pack $ aHash]

eth_getUncleCountByBlockNumber :: Int -> Int -> Value 
eth_getUncleCountByBlockNumber (anId) (aNumber) = 
  createRPCRequest
    defaultEthMethodParameters
    "getUncleCountByBlockNumber"
    anId
    [String . pack . convertIntToHex $ aNumber]

eth_getCode :: Int -> Address -> BlockQuantity -> Value
eth_getCode anId (Address anAddress) defaultQuantity = 
  createRPCRequest
    defaultEthMethodParameters
    "getCode"
    anId
    [String . pack . convertToAddress $ anAddress]

eth_sign :: Int -> Address -> Hash -> Value 
eth_sign anId (Address anAddress) (Hash sha3Hash) = 
  createRPCRequest
    defaultEthMethodParameters
    "sign" 
    anId
    [String . pack . convertToAddress $ anAddress 
    , String . pack $ sha3Hash ]

type Gas = Int
type GasPrice = Int

eth_sendTransaction :: Int -> Address -> Address -> Gas -> GasPrice -> Int -> Address -> Int -> Value
eth_sendTransaction anId (Address from) (Address to) gas gasPrice quantity (Address dataString) aNonce = 
  createRPCRequest 
    defaultEthMethodParameters
    "sendTransaction"
    anId $
      (String . pack) <$> 
        [
          convertToAddress from
          , convertToAddress to 
          , convertIntToHex gas
          , convertIntToHex gasPrice
          , convertIntToHex quantity 
          , convertToAddress dataString
        ] 

eth_sendRawTransaction = undefined 
eth_call = undefined 
eth_estimateGas = undefined 
eth_getBlockByHash = undefined
eth_getTransactionByHash = undefined
eth_getTransactionByBlockHashAndIndex = undefined 
eth_getTransactionByBlockNumberAndIndex = undefined 
eth_getTransactionReceipt = undefined
eth_getUncleByBlockHashAndIndex = undefined 
eth_getUncleByBlockNumberAndIndex = undefined 
eth_getCompilers = undefined
eth_compileSolidity = undefined
eth_compileLLL = undefined 
eth_compileSerpent = undefined 
eth_newFilter = undefined
eth_newBlockFilter = undefined
eth_newPendingTransactionFilter = undefined
eth_uninstallFilter = undefined
eth_getFilterChanges = undefined 
eth_getFilterLogs = undefined
eth_getLogs = undefined
eth_getWork = undefined 
eth_submitWork = undefined
eth_submitHashRate = undefined
db_putString = undefined
db_getString = undefined 
db_putHex = undefined 
db_getHex =  undefined 
shh_version = undefined 
shh_post = undefined 
shh_newIdentity = undefined
shh_hasIdentity = undefined 
shh_newGroup = undefined
shh_addToGroup = undefined 
shh_newFilter = undefined 
shh_uninstallFilter = undefined 
shh_getFilterChanges = undefined 
shh_getMessages = undefined



convertIntToHex :: Int -> String
convertIntToHex anId = printf "0x%x" anId
convertToAddress :: String -> String
convertToAddress anAddress = printf "0x%s" anAddress
