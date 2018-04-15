{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Zya.Ethereum.Internal.Types.RPCRequest where

import Data.Aeson
import Text.Printf
import Data.Text
import Text.ParserCombinators.Parsec 
import Data.Zya.Ethereum.Internal.Types.Common
import Data.Zya.Utils.JsonRPC



----- Eth client JSON Rpc methods --- 

clientVersion :: Int -> Value
clientVersion anId = 
    createRPCRequestWithDefaults "clientVersion" anId []

net_peerCount :: Int -> Value
net_peerCount anId = 
    createRPCRequest defaultNetMethodParameters "peerCount" anId []

net_listening :: Int -> Value
net_listening anId = 
    createRPCRequest defaultNetMethodParameters "listening" anId []

sha3 :: Param -> Int -> Value
sha3 aParam anId = 
  createRPCRequestWithDefaults "sha3" anId [aParam]

net_version :: Int -> Value
net_version anId = 
  createRPCRequest 
    defaultNetMethodParameters 
    "version" anId []

eth_protocolVersion :: Int -> Value
eth_protocolVersion anId = 
  createRPCRequest
    defaultEthMethodParameters 
    "protocolVersion" anId []

eth_syncing :: Int -> [Param] -> Value
eth_syncing anId = 
  createRPCRequest
    defaultEthMethodParameters
    "syncing" anId
-- Fix this
eth_coinbase :: Int -> [Param] -> Value 
eth_coinbase anId = 
  createRPCRequest
    defaultEthMethodParameters
    "coinbase" anId    

-- This call is not working.
eth_compilers :: Int -> [Param] -> Value
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
eth_getBlockTransactionCountByHash anId (Hash aHash) _ = 
  createRPCRequest defaultEthMethodParameters
    "getBlockTransactionCountByHash"
    anId
    [
      String . pack $ aHash
    ]

eth_getBlockTransactionCountByNumber :: Int -> Integer -> BlockQuantity -> Value 
eth_getBlockTransactionCountByNumber anId transactionId _ = 
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

eth_getUncleCountByBlockNumber :: Int -> Integer -> Value 
eth_getUncleCountByBlockNumber (anId) (aNumber) = 
  createRPCRequest
    defaultEthMethodParameters
    "getUncleCountByBlockNumber"
    anId
    [String . pack . convertIntToHex $ aNumber]

eth_getCode :: Int -> Address -> BlockQuantity -> Value
eth_getCode anId (Address anAddress) _ = 
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

type Gas = Integer
type GasPrice = Integer
type TransactionValue = Integer
type NonceValue = Integer

eth_sendTransaction :: Int -> Address -> Address -> Gas -> GasPrice -> TransactionValue -> Address -> NonceValue -> Value
eth_sendTransaction anId (Address from') (Address to') gas' gasPrice' quantity' (Address dataString) _ = 
  createRPCRequest 
    defaultEthMethodParameters
    "sendTransaction"
    anId $
        [
          object[ "from" .= (String . pack $ convertToAddress from')
          ,  "to" .= (String . pack . convertToAddress $ to')
          , "gas" .= (String .pack . convertIntToHex $ gas')
          , "gasPrice" .= (String .pack . convertIntToHex $ gasPrice')
          , "value" .= (String . pack . convertIntToHex $ quantity')
          , "data" .= (String . pack . convertToAddress $ dataString)
          ]
        ] 

type EthData = Text 

eth_sendRawTransaction :: Int -> EthData -> Value 
eth_sendRawTransaction anId aData = 
  createRPCRequest 
    defaultEthMethodParameters
    "sendRawTransaction"
    anId $[String aData]

eth_call :: Int -> Address -> Address -> Gas -> GasPrice -> Integer -> EthData -> Value
eth_call anId (Address from') (Address to') gas' gasPrice' quantity' encodedData' = 
  createRPCRequest 
    defaultEthMethodParameters 
    "call"
    anId $ 
        [
          object[ "from" .= (String . pack . convertToAddress $ from')]
          , object["to" .= (String . pack . convertToAddress $ to')]
          , object["gas" .= (String . pack . convertIntToHex $ gas')]
          , object["gasPrice" .= (String . pack . convertIntToHex $ gasPrice')]
          , object["value" .= (String . pack . convertIntToHex $ quantity')]
          , object["data" .= (String encodedData')]
        ]
eth_estimateGas :: Int -> Address -> Address -> Gas -> GasPrice -> Integer -> EthData -> Value 
eth_estimateGas anId (Address from') (Address to') gas' gasPrice' quantity' encodedData' = 
  createRPCRequest 
    defaultEthMethodParameters 
    "estimateGas"
    anId $ 
        [
          object[ "from" .= (String . pack . convertToAddress $ from')]
          , object["to" .= (String . pack . convertToAddress $ to')]
          , object["gas" .= (String . pack . convertIntToHex $ gas')]
          , object["gasPrice" .= (String . pack . convertIntToHex $ gasPrice')]
          , object["value" .= (String . pack . convertIntToHex $ quantity')]
          , object["data" .= (String encodedData')]
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

eth_getTransactionByBlockHashAndIndex :: EthData -> Value
eth_getTransactionByBlockHashAndIndex = undefined 

eth_getTransactionByBlockNumberAndIndex :: EthData -> Value
eth_getTransactionByBlockNumberAndIndex = undefined

eth_getTransactionReceipt :: EthData -> Value 
eth_getTransactionReceipt = undefined

eth_getUncleByBlockHashAndIndex :: EthData -> Value
eth_getUncleByBlockHashAndIndex = undefined 

eth_getUncleByBlockNumberAndIndex :: EthData -> Value
eth_getUncleByBlockNumberAndIndex = undefined 

eth_getCompilers :: EthData -> Value
eth_getCompilers = undefined

eth_compileSolidity :: EthData -> Value
eth_compileSolidity = undefined

eth_compileLLL :: EthData -> Value
eth_compileLLL = undefined 

eth_compileSerpent :: EthData -> Value
eth_compileSerpent = undefined 

eth_newFilter :: EthData -> Value
eth_newFilter = undefined

eth_newBlockFilter :: EthData -> Value
eth_newBlockFilter = undefined

eth_newPendingTransactionFilter :: EthData -> Value
eth_newPendingTransactionFilter = undefined

eth_uninstallFilter :: EthData -> Value
eth_uninstallFilter = undefined

eth_getFilterChanges :: EthData -> Value
eth_getFilterChanges = undefined 

eth_getFilterLogs :: EthData -> Value
eth_getFilterLogs = undefined

eth_getLogs :: EthData -> Value
eth_getLogs = undefined

eth_getWork :: EthData -> Value
eth_getWork = undefined 

eth_submitWork :: EthData -> Value
eth_submitWork = undefined

eth_submitHashRate :: EthData -> Value
eth_submitHashRate = undefined

db_putString :: EthData -> Value
db_putString = undefined

db_getString :: EthData -> Value
db_getString = undefined 

db_putHex :: EthData -> Value
db_putHex = undefined 

db_getHex :: EthData -> Value
db_getHex =  undefined 

shh_version :: EthData -> Value
shh_version = undefined 

shh_post :: EthData -> Value
shh_post = undefined 

shh_newIdentity :: EthData -> Value
shh_newIdentity = undefined

shh_hasIdentity :: EthData -> Value 
shh_hasIdentity = undefined 

shh_newGroup :: EthData -> Value 
shh_newGroup = undefined

shh_addToGroup :: EthData -> Value
shh_addToGroup = undefined 

shh_newFilter :: EthData -> Value
shh_newFilter = undefined 

shh_uninstallFilter :: EthData -> Value
shh_uninstallFilter = undefined

shh_getFilterChanges :: EthData -> Value  
shh_getFilterChanges = undefined 

shh_getMessages :: EthData -> Value
shh_getMessages = undefined



convertIntToHex :: Integer -> String
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

