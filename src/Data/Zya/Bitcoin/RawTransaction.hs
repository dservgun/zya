{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Zya.Bitcoin.RawTransaction where
import Data.Aeson
import Data.Map as Map
import Data.Scientific
import Data.Text as Text
import Data.Zya.Bitcoin.Common
import GHC.Generics

{-- | Raw transactions return greater detail about a given transaction and 
      can be queried without having to load the address or the account in
      the wallet. 
--}


data RawTransaction = RawTransaction {
  __txid :: Text
  , __hash :: Text
  , __version :: Integer
  , __size :: Integer
  , __vsize :: Integer
  , __locktime :: Integer
  , __vin :: [ValueIn]
  , __vout :: [ValueOut]
  , __hex :: Text
  , __blockHash :: Text
  , __confirmations :: Integer
  , __time :: Integer
  , __blockTime :: Integer
} deriving(Show, Generic)




instance FromJSON RawTransaction where 
  parseJSON = withObject "rawtransaction" $ \o -> do 
    __txid <- o .: "txid"
    __hash <- o .: "hash"
    __version <- o .: "version"
    __size <- o .: "size" 
    __vsize <- o .: "vsize"
    __locktime <- o .: "locktime"
    __vin <- o .: "vin"
    __vout <- o .: "vout"
    __hex <- o .: "hex"
    __blockHash <- o .: "blockhash"
    __confirmations <- o .: "confirmations"
    __time <- o .: "time"
    __blockTime <- o .: "blocktime"
    return RawTransaction{..}


instance ToJSON RawTransaction where 
  toJSON(RawTransaction t h ver siz vs lt vin vout hex blockh conf time blockTime) = 
    object [
      "txid" .= t 
      , "hash" .= h 
      , "version" .= ver
      , "size" .= siz
      , "vsize" .= vs 
      , "locktime" .= lt 
      , "vin" .= vin
      , "vout" .= vout
      , "hex" .= hex
      , "blockhash" .= blockh
      , "confirmations" .= conf 
      , "time" .= time
      , "blocktime" .= blockTime
    ]


-- Need to understand how to fold this stuff.
groupFormatCSV (AccountAddress acc) (Address addr) (RawTransaction t h v size vsize lT vin vout _ _ confirmations time blockTime) = 
  --Prelude.map formatCSV (\a -> Map.union footer a) valueOuts
  Prelude.map (\x -> formatCSVWithM x) valueOuts
  where
    valueOuts = fmap (\a -> Map.union envelope a) 
                  $ Prelude.map prepareCSV vout
    footer :: Map Int Text
    footer = Map.fromList $ 
      Prelude.zipWith (,) [-5, -4, -3, -2, -1]
        $ Text.pack <$> [show acc, show addr, show confirmations, show time, show blockTime]
    envelope = footer


rawTransactionAsCSV :: AccountAddress -> Address -> RawTransaction -> Text 
rawTransactionAsCSV account address = \a -> Text.unlines $ groupFormatCSV account address a