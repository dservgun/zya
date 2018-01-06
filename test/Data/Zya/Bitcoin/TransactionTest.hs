{-# LANGUAGE RecordWildCards #-}
module Data.Zya.Bitcoin.TransactionTest where

import Control.Applicative 
import Data.Aeson
import Data.Scientific
import Data.Zya.Bitcoin.Transaction
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.QuickCheck as QC

instance Arbitrary Transaction where 
  arbitrary = Transaction
                <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary



instance Arbitrary AccountAddress where 
  arbitrary = AccountAddress <$> arbitrary

instance Arbitrary Address where 
  arbitrary = Address <$> arbitrary  

instance Arbitrary TransactionSummary where 
  arbitrary = TransactionSummary
                <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary

prop1 (aTransaction@(Transaction{..})) = 
    (Data.Aeson.Success aTransaction) == (fromJSON . toJSON $ aTransaction)

prop2 (transSummary@(TransactionSummary{..})) =
    Data.Aeson.Success transSummary == (fromJSON . toJSON $ transSummary)

jsonProperties = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "Transaction = toJSON fromJSON " prop1
  , QC.testProperty "TransactionSummary  = toJSON fromJSON" prop2
  ]
