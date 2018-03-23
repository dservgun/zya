{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Data.Zya.Bitcoin.Common where

import Data.Aeson
import Data.Monoid((<>))
import Data.Maybe
import Data.Aeson.Types
import Data.Map as Map
import Data.Set as Set
import Data.Scientific
import Data.Text as Text
import GHC.Generics
import Data.Zya.Bitcoin.Block
import System.IO(openFile, IOMode(..), hGetContents)
import Control.Exception(bracket)


newtype RequestId = RequestId {id :: Integer} deriving(Show, Eq, Generic)
newtype AccountAddress = AccountAddress {_accountAddress :: Text} deriving(Show, Eq, Generic, Ord)
newtype Address = Address {_unaddress :: Text} deriving (Show, Eq, Generic, Ord)
newtype UserName = UserName {_uName :: String} deriving(Show) 
newtype Password = Password {_uPassword :: String} deriving(Show)


newtype BlockQuery = 
  BlockQuery { _b :: (BlockHeight, [Address])} deriving (Show, Eq, Generic)


instance Num RequestId where 
  RequestId a + RequestId b = RequestId (a + b)
  fromInteger = RequestId 



instance FromJSON Address where 
  parseJSON a = do 
    v <- modifyFailure ((errorMessageReport "Failed to parse address" a) ++) (parseString a)
    return (Address v)

instance ToJSON Address where
  toJSON (Address add) = String add


type KeyType = Text

data ScriptPubKey = ScriptPubKey {
  __asm :: Text
  , __hex :: Text
  , __reqSigs :: Maybe Integer
  , __type :: KeyType
  , __keyAddresses :: [Address]
} deriving(Show, Generic)


instance ToJSON ScriptPubKey where 
  toJSON (ScriptPubKey a h r t addresses) = 
    object
      [
        "asm" .= a 
        , "hex" .= h 
        , "reqSigs" .= r 
        , "type" .= t 
        , "addresses" .= addresses
      ]    

instance FromJSON ScriptPubKey where 
  parseJSON = \x -> 
    modifyFailure ((errorMessageReport "Failed to parse script pub key" x) ++ ) 
      (parseScriptPubKeyObject x)


parseScriptPubKeyObject = withObject "script pub key" $ \o -> do 
    __asm <- (o .:? "asm") -- return an empty string.
    __hex <- o .: "hex"
    __reqSigs <- o .:? "reqSigs"
    __type <- o .: "type"
    __keyAddresses <- o .: "addresses"
    return $ 
      ScriptPubKey 
        (fromMaybe "defAsm" __asm) __hex __reqSigs __type __keyAddresses

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
  parseJSON = 
    \x -> modifyFailure ("Failed to parse script sig" ++ ) (parseScriptSigObject x)

parseScriptSigObject = withObject "script sig" $ \o -> do
    __scriptSigAsm <- o .: "asm" 
    __scriptSigHex <- o .: "hex" 
    return ScriptSig{..}
-- Value in is to maintain a chain, that can be used to build the chain
-- upward. So, vin-> vouts must contain the current transaction.
data ValueIn = ValueIn {
  __inputTxId :: Maybe Text
  , __inputVout :: Maybe Integer
  , __scriptSig :: Maybe ScriptSig
  , __sequence :: Maybe Integer
} deriving(Show)

data ValueOut = ValueOut {
  __value :: Scientific
  , __n :: Integer
  , __scriptPubKey :: ScriptPubKey
} deriving(Show)


instance FromJSON ValueOut where
  parseJSON = \x -> modifyFailure((errorMessageReport "Failed to parse ValueOut" x) ++) 
                    (parseJSONValueOut x)

instance ErrorReporter Value where 
  errorMessageReport customError aValue = customError ++ " " ++ (show aValue)

class ErrorReporter a where
  errorMessageReport :: (Show a) => String -> a -> String 

instance ErrorReporter ValueOut where 
  errorMessageReport customError v = customError ++ " " ++ show v

instance ErrorReporter ScriptPubKey where 
  errorMessageReport customError v = customError ++ ":" ++ show v

parseJSONValueOut = withObject "valueout" $ \o -> do 
  __value <- o .: "value"
  __n <- o .: "n"
  __scriptPubKey <- o .: "scriptPubKey"
  return ValueOut{..}

instance ToJSON ValueOut where 
  toJSON (ValueOut v n s) = 
      object [
        "value" .= v
        , "n" .= n 
        , "scriptPubKey" .= s 
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
  parseJSON = 
    \x -> modifyFailure 
            ((errorMessageReport "Failed to parse ValueIn " x )++ )
            (parseJSONValueIn x)

parseJSONValueIn = withObject "valuein" $ \o -> do 
  __inputTxId <- o .:? "txid" 
  __inputVout <- o .:? "vout"
  __scriptSig <- o .:? "scriptSig"
  __sequence <- o .:? "sequence"
  return ValueIn{..}    


class CSVFormatter a where 
  -- | A formatter returning the ordering and the text to go with 
  -- | it. 
  prepareCSV :: a -> Map Int Text

formatCSVWithM :: Map Int Text -> Text
formatCSVWithM aRow = 
    Text.intercalate "," orderedList
    where
      orderedList = snd <$> Map.toAscList aRow

formatCSV :: (CSVFormatter a) => a -> Text
formatCSV aRow = 
    Text.intercalate "," orderedList
    where
      orderedList = snd <$> Map.toAscList (prepareCSV aRow)

instance CSVFormatter AccountAddress where 
  prepareCSV (AccountAddress anAddress) = Map.fromList[(1, anAddress)]

instance CSVFormatter Address where
  prepareCSV (Address anAddress) = Map.fromList[(1, anAddress)]

instance CSVFormatter ValueOut where 
  prepareCSV (ValueOut v n sP) = 
    Map.fromList $ 
        Prelude.zipWith (,) 
          [1..]
          [Text.pack $ show v, mergeScriptAddresses sP]



mergeScriptAddresses :: ScriptPubKey -> Text 
mergeScriptAddresses (ScriptPubKey _ _ _ _ addresses) = 
  Text.intercalate "," $ _unaddress <$> Prelude.filter (\a@(Address add) -> add /= "") addresses


filterAddresses :: [Address] -> ScriptPubKey -> ScriptPubKey 
filterAddresses queryList (ScriptPubKey a b c d addresses) = 
    ScriptPubKey a b c d $ Set.toList $ Set.intersection addressSet querySet
    where
      querySet = Set.fromList queryList 
      addressSet = Set.fromList addresses

filterAddressesValueOut :: [Address] -> ValueOut -> ValueOut 
filterAddressesValueOut queryList (ValueOut n p scriptPubKey) = 
    ValueOut n p $ filterAddresses queryList scriptPubKey

instance FromJSON AccountAddress where
  parseJSON a = do 
    v <- modifyFailure ("Failed to parse AccountAddress" ++) (parseString a)
    return $ AccountAddress v
instance ToJSON AccountAddress where 
  toJSON (AccountAddress add) = String add



readInputLines :: FilePath -> IO [String] 
readInputLines aFile = do
  bracket (openFile aFile ReadMode) 
          (\_ -> return()) $ \h -> do 
            contents <- hGetContents h
            return $ Prelude.lines contents


filterVOWithAddresses :: ValueOut -> Bool 
filterVOWithAddresses (ValueOut a b (ScriptPubKey _ _ _ _ addresses)) = 
  case addresses of
    [] -> False
    _ -> True
