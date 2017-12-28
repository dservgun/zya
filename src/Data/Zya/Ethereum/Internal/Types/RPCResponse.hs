{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Zya.Ethereum.Internal.Types.RPCResponse where

import Data.Aeson
import Text.Printf
import Data.Zya.Ethereum.Internal.Types.Common
data RPCResponse = RPCResponse {
  id :: Int
  , jsonRpc :: String
  , result :: Value
}

data SyncResponse = 
  SyncResponse {
    startingBlock :: Integer 
    , currentBlock :: Integer
    , knownStates :: Integer
    , pulledStates :: Integer
    , highestBlock :: Integer
  } deriving(Show)

instance ToJSON SyncResponse where
  toJSON (SyncResponse s c h _ _) = 
    object [
        "startingBlock" .= sBlock s
      , "currentBlock" .= sBlock c
      , "highestBlock" .= sBlock h]
    where
      sBlock :: Integer -> String 
      sBlock = printf "0x%x" s

instance FromJSON SyncResponse where 
  parseJSON = withObject "SyncResponse" $ \w -> do
      v <- w .: "result"
      s <- v .: "startingBlock"
      c <- v .: "currentBlock"
      k <- v .: "knownStates" 
      p <- v .: "pulledStates" 
      h <- v .: "highestBlock"
      return $ SyncResponse (read s) (read c) (read k) (read p) (read h)

eth_syncResponse :: Value -> Result SyncResponse
eth_syncResponse = fromJSON

eth_blockByNumberResponse :: Value -> Result BlockByHash
eth_blockByNumberResponse = fromJSON