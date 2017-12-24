{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Zya.Utils.Validation where 

import Data.Monoid
import Data.Text


newtype ErrorMessage = ErrorMessage {message :: [Text]} deriving (Show)

instance Monoid ErrorMessage where 
  mempty = ErrorMessage []
  mappend (ErrorMessage e1) (ErrorMessage e2) = ErrorMessage (e1 <> e2)
-- applications
deleteIfNegative :: (Show a, Num a, Ord a) => a -> Either ErrorMessage a 
deleteIfNegative a = 
  if a < 0 then 
    (Left (ErrorMessage [pack . show $ a]))
  else Right a


newtype Validation e a = 
  Validation {getValidation :: Either e a}
  deriving(Functor, Show) 


instance Monoid e => Applicative (Validation e) where 
  pure = Validation . Right
  Validation a <*> Validation b = 
    Validation $ 
      case a of 
        Right va -> fmap va b 
        Left ea -> either (Left . mappend ea) (const $ Left ea) b


