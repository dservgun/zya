{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Zya.Bitcoin.Common where

import Data.Text
import Data.Scientific
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
newtype RequestId = RequestId {id :: Text} deriving(Show, Eq)
newtype AccountAddress = AccountAddress {_accountAddress :: Text} deriving(Show, Eq)
newtype Address = Address {_unaddress :: Text} deriving (Show, Eq, Generic)

parseString :: Value -> Parser Text
parseString = withText "string" $ return

instance FromJSON Address where 
  parseJSON a = do 
    v <- parseString a
    return (Address v)

instance ToJSON Address where
  toJSON (Address add) = String add


type KeyType = Text

data ScriptPubKey = ScriptPubKey {
  __asm :: Text
  , __scriptPubKeyHex :: Text
  , __reqSigs :: Integer
  , __type :: KeyType
  , __keyAddresses :: [Address]
} deriving(Show)


instance ToJSON ScriptPubKey where 
  toJSON (ScriptPubKey a s r t addresses) = 
    object
      [
        "asm" .= a 
        , "hex" .= s 
        , "reqSigs" .= r 
        , "type" .= t 
        , "addresses" .= addresses
      ]    


instance FromJSON ScriptPubKey where 
  parseJSON = withObject "script pub key" $ \o -> do 
    __asm <- o .: "asm"
    __scriptPubKeyHex <- o .: "hex"
    __reqSigs <- o .: "reqSigs"
    __type <- o .: "type"
    __keyAddresses <- o .: "addresses"
    return ScriptPubKey{..}

-- field names prefixed with type name to prevent duplicate declarations
-- error.
data ScriptSig = ScriptSig {
  __scriptSigAsm :: Text
  , __scriptSigHex :: Text
} deriving(Show)

instance ToJSON ScriptSig where 
  toJSON (ScriptSig asm hex) = 
    object [
      "asm" .= asm
      , "hex" .= hex
    ]

instance FromJSON ScriptSig where 
  parseJSON = withObject "script sig" $ \o -> do
    __scriptSigAsm <- o .: "asm" 
    __scriptSigHex <- o .: "hex" 
    return ScriptSig{..}
-- Value in is to maintain a chain, that can be used to build the chain
-- upward. So, vin-> vouts must contain the current transaction.
data ValueIn = ValueIn {
  __inputTxId :: Text
  , __inputVout :: Integer
  , __scriptSig :: ScriptSig
  , __sequence :: Integer
} deriving(Show)

data ValueOut = ValueOut {
  __value :: Scientific
  , __n :: Integer
  , __scriptPubKey :: ScriptPubKey
} deriving(Show)


instance FromJSON ValueOut where
  parseJSON = withObject "valueout" $ \o -> do 
    __value <- o .: "value"
    __n <- o .: "n"
    __scriptPubKey <- o .: "ScriptPubKey"
    return ValueOut{..}

instance ToJSON ValueOut where 
  toJSON (ValueOut v n s) = 
      object [
        "value" .= v
        , "n" .= n 
        , "ScriptPubKey" .= s 
      ]


instance ToJSON ValueIn where 
  -- sequence is a function that gets picked up by sublime haskell.
  toJSON (ValueIn iTx ivout scriptSig sequenc) = 
    object [
      "txid" .= iTx 
      , "vout" .= ivout
      , "scriptSig" .= scriptSig
      , "sequence" .= sequenc
    ]

instance FromJSON ValueIn where 
  parseJSON = withObject "valuein" $ \o -> do 
    __inputTxId <- o .: "txid" 
    __inputVout <- o .: "vout"
    __scriptSig <- o .: "scriptSig"
    __sequence <- o .: "sequence"
    return ValueIn{..}    
