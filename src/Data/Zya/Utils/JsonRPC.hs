{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Zya.Utils.JsonRPC where

import Data.Aeson
import Data.Monoid



{-- | Send and receive 

--}


newtype Request = Request {_unRequest :: Int} deriving(Show)
newtype StoragePosition = StoragePosition {_unSP :: Integer} deriving(Show)

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


joinResponse :: Maybe(Result a) -> Result a
joinResponse (Just a) = a 
joinResponse Nothing = Error "No response.."

