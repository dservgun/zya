{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}

module Data.Zya.Core.Internal.CSVFormatter where 
import Control.Comonad
import Data.Monoid
import Prelude
import Test.QuickCheck
import Data.Foldable
import GHC.Generics
import Data.Set as Set
import Data.Text

newtype Column = Column {_unCol :: Text} deriving (Eq, Ord, Show)
newtype Value = Value {_unVal :: Text} deriving (Eq, Ord, Show)

class CSVFormatter a where
  output :: a -> Set Value
  default output :: (Generic a, GCSVFormatter(Rep a)) => a -> Set Value
  output a = goutput(from a)

class GCSVFormatter f where 
  goutput :: f a -> Set Value


instance GCSVFormatter U1 where 
  goutput U1 = Set.empty 


instance (GCSVFormatter a, GCSVFormatter b) => GCSVFormatter (a :*: b) where 
  goutput (a :*: b) = Set.union (goutput a) (goutput b)

instance (GCSVFormatter a, GCSVFormatter b) => GCSVFormatter (a :+: b) where 
  goutput (L1 x) = goutput x
  goutput (R1 x) = goutput x

instance (GCSVFormatter a) => GCSVFormatter (M1 i c a) where 
  goutput (M1 x) = goutput x

instance (CSVFormatter  a) => GCSVFormatter (K1 i a) where 
  goutput (K1 x) = Data.Zya.Core.Internal.CSVFormatter.output x


-- | Some instances

instance CSVFormatter [Char] where 
  output aString = Set.fromList [Value . pack $ aString]

instance CSVFormatter Text where 
  output aText = Set.fromList [Value aText]

instance CSVFormatter Int where 
  output aText = Set.fromList [Value . pack . show $ aText]

instance CSVFormatter Integer where 
  output anInteger = Set.fromList[Value . pack . show $ anInteger]

