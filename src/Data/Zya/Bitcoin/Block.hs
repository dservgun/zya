{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}


module Data.Zya.Bitcoin.Block where


import Data.Aeson
import Data.Monoid((<>))
import Data.Aeson.Types
import Data.Map as Map
import Data.Scientific
import Data.Text as Text
import GHC.Generics
import Text.Printf(printf)

parseScientific :: Value -> Parser Scientific 
parseScientific = withScientific "blocks number" $ return

parseInteger :: Value -> Parser Integer 
parseInteger aValue = do 
  x <- parseString aValue
  --let y = read $ Text.unpack (x :: Text)
  return $ read $ Text.unpack $ "0x" <> x 

parseString :: Value -> Parser Text
parseString = withText "string" $ return

newtype BlockHash = BlockHash {_unblockHash :: Integer} deriving (Show, Eq, Generic)
newtype BlockHeight = BlockHeight {_unblock :: Int} deriving (Show, Eq, Generic)
newtype BlockWeight = BlockWeight {_unblockH :: Int} deriving (Show, Eq, Generic)


instance Num BlockHeight where 
  BlockHeight a + BlockHeight b = BlockHeight (a + b)
  fromInteger = BlockHeight . fromIntegral

instance Enum BlockHeight where 
  toEnum = BlockHeight 
  fromEnum (BlockHeight a) = a

instance FromJSON BlockHeight where 
  parseJSON a = do 
    v <- parseScientific a
    if (isInteger v) then
      return $ maybe (BlockHeight 0) (\a -> BlockHeight a) $ toBoundedInteger v
    else
      return $ BlockHeight (-1)

instance FromJSON BlockWeight where 
  parseJSON a = do 
    v <- parseScientific a
    if (isInteger v) then
      return $ maybe (BlockWeight 0) (\a -> BlockWeight a) $ toBoundedInteger v
    else
      return $ BlockWeight (-1)

instance FromJSON BlockHash where 
  parseJSON a = do
    v <- parseInteger a 
    return $ BlockHash v

{-- | Query all transactions for an address inside a block.
--}

-- | The bitcoinblock 
data Block = Block {
  blockHash :: BlockHash , blockConfirmations :: Integer
  , strippedSize :: Integer , size :: Integer
  , weight :: BlockWeight , height :: BlockHeight
  , blockVersion :: Integer , merkleRoot :: Text
  , transactionList :: [Text]
  , time :: Integer , medianTime  :: Integer
  , nonce :: Integer , bits :: String, difficulty :: Scientific
  , chainwork :: String, previousBlockHash :: String
  , nextBlockHash :: String -- BlockHash
} deriving (Show, Eq, Generic)




instance FromJSON Block where 
  parseJSON = 
      \x -> 
        modifyFailure("Failed parsing block" ++) $ (parseBlockObject x)

-- Reads better.
parseBlockObject = withObject "parse block" $ \o -> do 
      h <- o .: "hash"
      conf <- o .: "confirmations"
      str <- o .: "strippedsize"
      size <- o .: "size"
      weight <- o .: "weight"
      height <- o .:"height"
      vers <- o .: "version"
      merkleRoot <- o .: "merkleroot"
      txList <- o .: "tx"
      time <- o .: "time"
      medianT <- o .: "mediantime"
      nonc <- o .: "nonce"
      bits <- o .: "bits"
      difficulty <- o .: "difficulty"
      chainwork <- o .: "chainwork"
      prevBlock <- o .: "previousblockhash"
      nextBlock <- o .: "nextblockhash"
      return $ Block h conf str 
              size weight height vers 
              merkleRoot txList time 
              medianT nonc bits difficulty
              chainwork prevBlock nextBlock
