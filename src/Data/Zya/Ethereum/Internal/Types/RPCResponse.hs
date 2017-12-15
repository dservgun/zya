{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Ethereum.Internal.Types.RPCResponse where

import Data.Aeson

data RPCResponse = RPCResponse {
  id :: Int
  , jsonRpc :: String
  , result :: Value
}

