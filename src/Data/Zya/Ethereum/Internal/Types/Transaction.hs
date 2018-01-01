{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Zya.Ethereum.Internal.Types.Transaction 
  (
    Transaction(..)
    , transactionOutput
    , OutputFormat(..)
  )
where
import Data.Aeson
import Data.Monoid
import GHC.Generics
import Data.HashMap.Strict as HM
import Data.Text as Text

data OutputFormat = CSV Text | JSON deriving (Show)

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
    , from :: Integer -- TODO revisit.
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
                  hashS <- v .: "hash"
                  nonceS <- v .: "nonce"
                  blockHashS <- v .: "blockHash" 
                  blockNumberS <- v .: "blockNumber" 
                  transactionIndexS <- v .: "transactionIndex"                  
                  frJ <- v .: "from"
                  frS <- case frJ of 
                    String s -> return (Text.unpack s)
                    _ -> return ("0x00000"  :: String)
                  toAJ <- v .: "to"
                  toA <- case toAJ of 
                    String s -> return (Text.unpack s)                    
                    _ -> return ("0x00000"  :: String)
                  valueS <- v .: "value"
                  gasPriceS <- v .: "gasPrice"
                  gasS <- v .: "gas" 
                  inputS <- return "0x00" -- v .: "input"
            --      replayProtected <- v .: "replayProtected"
            --      chainId <- v .: "chainId"
                  return $ Transaction (read hashS) (read nonceS) (read blockHashS) 
                        (read blockNumberS)
                        (read transactionIndexS)
                        (read frS) 
                        (read toA)
                        (read valueS) (read gasPriceS)
                        (read gasS) (read inputS)
                        False Nothing -- need to fix this.
            _ -> return $ Transaction 0 0 0 
                            0 0 
                            0  -- from can be null, because the contract perhaps got created.
                            0 
                            0 
                            0 
                            0 
                            0 False Nothing
        Nothing -> do 
                hashS <- vOuter .: "hash"
                nonceS <- vOuter .: "nonce"
                blockHashS <- vOuter .: "blockHash" 
                blockNumberS <- vOuter .: "blockNumber" 
                transactionIndexS <- vOuter .: "transactionIndex"
                frJ <- vOuter .: "from"
                frS <- case frJ of 
                  String s -> return (Text.unpack s)
                  _ -> return ("0x00000"  :: String)

                toAJ <- vOuter .: "to"
                toA <- case toAJ of 
                  String s -> return (Text.unpack s)                    
                  _ -> return ("0x00000"  :: String)

                valueS <- vOuter .: "value"
                gasPriceS <- vOuter .: "gasPrice"
                gasS <- vOuter .: "gas" 
                inputS <- return "0x00" -- v .: "input"
          --      replayProtected <- v .: "replayProtected"
          --      chainId <- v .: "chainId"
                return $ Transaction (read hashS) (read nonceS) (read blockHashS) 
                      (read blockNumberS)
                      (read transactionIndexS)
                      (read frS) (read toA)
                      (read valueS) (read gasPriceS)
                      (read gasS) (read inputS)
                      False Nothing -- need to fix this.

instance ToJSON Transaction

transactionOutput :: Transaction -> OutputFormat -> Text
transactionOutput = 
  \t format -> Text.pack $ 
      (show . hash $ t) <> "," <> (show $ Data.Zya.Ethereum.Internal.Types.Transaction.from t) <> "," 
      <> (show $ Data.Zya.Ethereum.Internal.Types.Transaction.to t) <> "," <> (show $ value t)
      <> "," <> (show $ gasPrice t) <> "," <> (show $ gas t) <> "\n"
