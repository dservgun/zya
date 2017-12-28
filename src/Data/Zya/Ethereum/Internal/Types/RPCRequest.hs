{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Zya.Ethereum.Internal.Types.RPCRequest where

import Data.Aeson
import Text.Printf
import Data.Monoid
import Data.Text
import Text.ParserCombinators.Parsec 
import Control.Monad.IO.Class(liftIO)
import Data.Zya.Ethereum.Internal.Types.Common

newtype Request = Request {_unRequest :: Int} deriving(Show)
newtype StoragePosition = StoragePosition {_unSP :: Int} deriving(Show)

type Param = Value -- Need to confirm this
type Method = String 

methodPrefix :: String 
methodPrefix = "web3"


newtype Address = Address {_unAddress :: String} deriving(Show, Eq)
newtype Hash = Hash {_unHash :: String}


data DefaultParameters = DefaultParameters {
  defaultRpcVersion :: String
  , defaultMethodPrefix :: String
} deriving (Show) 


{--|
  The parameters are specific a type of request. Use this
  typeclass to capture the parameters. 

--}

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
  createRPCRequest defaultEthMethodParameters "getBlockByNumber" anId
    $ toEthParam blockQuantity details


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
  createRPCRequest defaultEthMethodParameters "blockNumber" anId []

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
        [
          object[ "from" .= (String . pack $ convertToAddress from)]
          , object[ "to" .= (String . pack . convertToAddress $ to)] 
          , object[ "gas" .= (String .pack . convertIntToHex $ gas)]
          , object[ "gasPrice" .= (String .pack . convertIntToHex $ gasPrice)]
          , object[ "value" .= (String . pack . convertIntToHex $ quantity)]
          , object[ "data" .= (String . pack . convertToAddress $ dataString)]
        ] 

type EthData = Text 

eth_sendRawTransaction :: Int -> EthData -> Value 
eth_sendRawTransaction anId aData = 
  createRPCRequest 
    defaultEthMethodParameters
    "sendRawTransaction"
    anId $[String aData]

eth_call :: Int -> Address -> Address -> Gas -> GasPrice -> Int -> EthData -> Value
eth_call anId (Address from) (Address to) gas gasPrice quantity encodedData = 
  createRPCRequest 
    defaultEthMethodParameters 
    "call"
    anId $ 
        [
          object[ "from" .= (String . pack . convertToAddress $ from)]
          , object["to" .= (String . pack . convertToAddress $ to)]
          , object["gas" .= (String . pack . convertIntToHex $ gas)]
          , object["gasPrice" .= (String . pack . convertIntToHex $ gasPrice)]
          , object["value" .= (String . pack . convertIntToHex $ quantity)]
          , object["data" .= (String encodedData)]
        ]
eth_estimateGas :: Int -> Address -> Address -> Gas -> GasPrice -> Int -> EthData -> Value 
eth_estimateGas anId (Address from) (Address to) gas gasPrice quantity encodedData = 
  createRPCRequest 
    defaultEthMethodParameters 
    "estimateGas"
    anId $ 
        [
          object[ "from" .= (String . pack . convertToAddress $ from)]
          , object["to" .= (String . pack . convertToAddress $ to)]
          , object["gas" .= (String . pack . convertIntToHex $ gas)]
          , object["gasPrice" .= (String . pack . convertIntToHex $ gasPrice)]
          , object["value" .= (String . pack . convertIntToHex $ quantity)]
          , object["data" .= (String encodedData)]
        ]

eth_getBlockByHash :: Int -> EthData -> Value 
eth_getBlockByHash anId ethData = 
  createRPCRequest
    defaultEthMethodParameters
    "getBlockByHash"
    anId $ 
      [
        String ethData
        , Bool True
      ]
eth_getTransactionByHash :: Int -> EthData -> Value 
eth_getTransactionByHash anId ethData = 
  createRPCRequest
    defaultEthMethodParameters
    "getTransactionByHash"
    anId $ 
      [
        String ethData
      ]

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
convertToAddress anAddress = anAddress

maxAddress :: Integer
maxAddress = 0xffffffffffffffffffffffffffffffffffffffff
-- Valid address is 20 bytes
validAddressC :: Integer -> Either String Integer
validAddressC anAddress =  
  if anAddress < maxAddress then
    Right anAddress
  else
    Left "Invalid address"


-- Read an integer from a string

integerParser :: GenParser Char st Integer
integerParser = do 
  digits <- many digit
  let val = (read digits :: Integer) 
  return val


parseInteger :: String -> Either ParseError Integer
parseInteger = parse integerParser "Integer parsing failed"

