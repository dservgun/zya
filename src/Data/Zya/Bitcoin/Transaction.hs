{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Zya.Bitcoin.Transaction where

import Data.Aeson
import Data.Aeson.Types
import Data.Char as C (toLower)
import Data.Map as Map
import Data.Monoid((<>))
import Data.Scientific
import Data.Text as Text
import Data.Zya.Bitcoin.Block as BCommon
import Data.Zya.Bitcoin.Common as BCommon
import GHC.Generics

{-- | 
{
  "amount": 0.00000000,
  "confirmations": 135726,
  "blockhash": "000000000000000005ad1ffb2b7d88e9820ee8b4d02b724577d9b54215fd0272",
  "blockindex": 614,
  "blocktime": 1437862727,
  "txid": "497086fb54fb8b36210730f9ad4e202054840520439ab94150b39252aad229fa",
  "walletconflicts": [
  ],
  "time": 1437862727,
  "timereceived": 1515132264,
  "bip125-replaceable": "no",
  "details": [
  ],
  "hex" : 
--}


data Transaction = Transaction{
  __amount :: Scientific
  , __confirmations :: Integer
  , __blockhash :: Text
  , __blocktime :: Integer
  , __txid :: Text
  , __walletconflicts :: [Text]
  , __time :: Integer
  , __timereceived :: Integer
  , __bip125Replaceable :: Text
  , __details :: [Text]
  , __hex :: Text
} deriving(Generic, Eq)

instance Show Transaction where 
  show (Transaction a c b bl tx _ time timeR bip _ _) = 
      "transaction " <> (show a) <> " : " <> (show c) <> " : " <> (show b) <> " : " <> (show bl) <> " : " <> (show tx)
                    <> " : " <> show time <> " : " <> show timeR 
instance FromJSON Transaction where
  parseJSON = 
      \x -> modifyFailure ("Failed to parse Transaction " ++) (parseJSONTransaction x)

parseJSONTransaction = withObject "transaction" $ \o -> do 
    __amount <- o .: "amount"
    __confirmations <- o .:? "confirmations" .!= 0 
    __blockhash <- o .: "blockhash"
    __blocktime <- o .: "blocktime"
    __txid <- o .: "txid"
    __walletconflicts <- o .: "walletconflicts"
    __time <- o .: "time"
    __timereceived <- o .: "timereceived" 
    __bip125Replaceable <- o .: "bip125-replaceable"
    __details <- o .:"details"
    __hex <- o .: "hex"
    return Transaction{..}

instance ToJSON Transaction where 
  toJSON (Transaction a c bh bt txid wc time timer bip det hex) = 
      object [
          "amount" .= a 
        , "confirmations" .= c 
        , "blockhash" .= bh
        , "blocktime" .= bt 
        , "txid" .= txid
        , "walletconflicts" .= wc 
        , "time" .= time
        , "timereceived" .= timer
        , "bip125-replaceable" .= bip
        , "details" .= det 
        , "hex" .= hex
      ]

data TransactionSummary = TransactionSummary {
  _amount :: Scientific
  , _address :: Address
  , _account :: AccountAddress
  , _confirmations :: Integer
  , _label :: Text
  , _transactions :: [Text]
} deriving(Show, Generic, Eq)





instance FromJSON TransactionSummary where 
  parseJSON = withObject "transaction summary" $ \o -> do 
    _amount <- o .: "amount" 
    _address <- o .: "address"
    _account <- o .: "account"
    _confirmations <- o .: "confirmations" 
    _label <- o .: "label" 
    _transactions <- o .: "txids"
    return TransactionSummary{..}


instance ToJSON TransactionSummary where 
  toJSON (TransactionSummary a add acc con la trans) = 
    object [
      "amount" .= a 
      , "address" .= add 
      , "account" .= acc
      , "confirmations" .= con 
      , "label" .= la 
      , "txids" .= trans
    ]
{-fromList [("error",Null),("id",String "1"),("result",Array 
  [Object (fromList [("amount",Number 0.0),("address",String "1AUkYaMBeLuRkfAbzvgP2KKNs9NkySUdu"),(
    "account",String "16R9ffu5BPcKHoy3dvZLQeghqHgHecEVWF"),("confirmations",Number 0.0),("txids",Array []),
-}



-- Constants

nullAddress :: String
nullAddress = "0000000000000000"


instance CSVFormatter Transaction where 
  prepareCSV (Transaction amount conf blockH blockT txId _ time timeR _ _ _) = 
    Map.fromList 
      $ Prelude.zipWith (,) [1 ..] fieldValueList
    where 
      fieldValueList = 
        (Text.pack) <$> 
                [
                show amount 
                , show conf
                , show blockH
                , show blockT
                , show txId
                , show time
                , show timeR]

