{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Zya.Bitcoin.CSVFormatterTest where

import Control.Applicative 
import Data.Aeson
import Data.Scientific
import Data.Text
import Data.Zya.Core.Internal.CSVFormatter 
import Data.Set as Set
import Data.Zya.Bitcoin.Common hiding(CSVFormatter)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.QuickCheck as QC


instance CSVFormatter RequestId
instance CSVFormatter Address
instance CSVFormatter AccountAddress
instance CSVFormatter ScriptPubKey
instance CSVFormatter [Address]

instance Arbitrary Address where 
  arbitrary = Address <$> arbitrary
instance Arbitrary ScriptPubKey where 
  arbitrary = ScriptPubKey
                <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary


