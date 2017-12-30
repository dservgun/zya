{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Zya.Ethereum.Internal.Types.Transaction where
import Data.Aeson
import GHC.Generics
import Data.HashMap.Strict as HM
data Transaction = 
  Transaction {
    -- | Hash of the transaction
    hash :: Integer
    -- | The number of transactions made by the sender prior to this one.
    , nonce :: Integer
    -- | Hash of the block where the transaction was in. Null when 
    -- pending.
    , blockHash :: Integer
    -- | The block number where the transaction was in. Pending when null
    , blockNumber :: Integer
    -- | Transaction index position in the block. Null when pending.
    , transactionIndex :: Integer
    -- | address of the sender 
    , from :: Integer
    -- | address of the receiver
    , to :: Integer
    -- | Value transferred in wei
    , value :: Integer
    -- | Gas price 
    , gasPrice :: Integer
    -- | amount of gas provided by the sender
    , gas :: Integer
    -- | The data sent along with the transaction
    , input :: Integer
    -- | Replay protection (EIP-155)
    , replayProtected :: Bool
    -- | Chain id if replay protected is true
    , chainId :: Maybe Integer
  }deriving(Generic, Show)

instance FromJSON Transaction where 
  parseJSON = withObject "Transaction" $ \vOuter -> do
      let rField = HM.lookup "result" vOuter
      case rField of 
        Just v1 -> do
          case v1 of 
            Object v -> do  
                  hash <- v .: "hash"
                  nonce <- v .: "nonce"
                  blockHash <- v .: "blockHash" 
                  blockNumber <- v .: "blockNumber" 
                  transactionIndex <- v .: "transactionIndex"
                  fr <- v .: "from"
                  toA <- v .: "to"
                  value <- v .: "value"
                  gasPrice <- v .: "gasPrice"
                  gas <- v .: "gas" 
                  input <- return "0x00" -- v .: "input"
            --      replayProtected <- v .: "replayProtected"
            --      chainId <- v .: "chainId"
                  return $ Transaction (read hash) (read nonce) (read blockHash) 
                        (read blockNumber)
                        (read transactionIndex)
                        (read fr) (read toA)
                        (read value) (read gasPrice)
                        (read gas) (read input)
                        False Nothing -- need to fix this.
        Nothing -> do 
                hash <- vOuter .: "hash"
                nonce <- vOuter .: "nonce"
                blockHash <- vOuter .: "blockHash" 
                blockNumber <- vOuter .: "blockNumber" 
                transactionIndex <- vOuter .: "transactionIndex"
                fr <- vOuter .: "from"
                toA <- vOuter .: "to"
                value <- vOuter .: "value"
                gasPrice <- vOuter .: "gasPrice"
                gas <- vOuter .: "gas" 
                input <- return "0x00" -- v .: "input"
          --      replayProtected <- v .: "replayProtected"
          --      chainId <- v .: "chainId"
                return $ Transaction (read hash) (read nonce) (read blockHash) 
                      (read blockNumber)
                      (read transactionIndex)
                      (read fr) (read toA)
                      (read value) (read gasPrice)
                      (read gas) (read input)
                      False Nothing -- need to fix this.

instance ToJSON Transaction