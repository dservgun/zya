{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Ethereum.Internal.Bookeeping.ReconTransaction
  (
    reconTransactions, ReconTransaction, address, intentionAmount, toCSV
  ) where

import Data.Csv 
import qualified Data.ByteString.Lazy as BL 
import qualified Data.Vector as V 
import Text.Printf as Printf
import Universum



data ReconTransaction = ReconTransaction {
  address :: Integer
  , intentionAmount :: Double 
} deriving (Show, Eq)


parseDouble :: String -> Double
parseDouble "NULL" = 0.0
parseDouble x = maybe 0.0 id $ readMaybe x

readReconTransactions :: FilePath -> IO [ReconTransaction]
readReconTransactions aFilePath =
  BL.readFile aFilePath >>= \l -> 
  case decode HasHeader l of 
    Left err -> print err >> return [] 
    Right v -> do 
        vector <-       
            V.forM (v) $ 
              \(_ :: String, _ :: String, rAddress :: String
                  , amount :: String, _ :: String, _ :: String, _ :: String, _ :: String) -> 
                return $ ReconTransaction (maybe (-1) id $ readMaybe rAddress) (parseDouble amount)
        return $ V.toList vector

reconTransactions :: FilePath -> IO [ReconTransaction]
reconTransactions reconFile = readReconTransactions reconFile 

toCSV :: ReconTransaction -> Text 
toCSV (ReconTransaction _address amount) = 
    toText $ ((Printf.printf "0x%x" _address)) <> ("," :: String) <> (show amount)