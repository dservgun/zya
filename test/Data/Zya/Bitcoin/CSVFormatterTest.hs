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

instance CSVFormatter RequestId
instance CSVFormatter Address
instance CSVFormatter AccountAddress
instance CSVFormatter ScriptPubKey
instance CSVFormatter [Address]
