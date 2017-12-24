{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TraversableTest where

import Data.Monoid
import Data.Text

data Tree a = Leaf a | Branch (Tree a) (Tree a)


instance Functor Tree where 
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f1 (Leaf l) = Leaf (f1 l) 
  fmap f1 (Branch lTree rTree) = Branch (f1 <$> lTree) (f1 <$> rTree)

instance Foldable Tree where 
  foldMap foldFunction (Leaf l) = foldFunction l 
  foldMap foldFunction (Branch l r) = (foldMap foldFunction l) <> (foldMap foldFunction r)


--instance Traversable Tree where 
instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Branch l r) = Branch <$> (traverse f l) <*> (traverse f r)

