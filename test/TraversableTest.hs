{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TraversableTest where

import Data.Monoid
import Data.Text
import Control.Monad
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

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving(Show)

{-instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose (fmap (fmap x))

instance (Applicative f, Applicative g) => Applicative (Compose f g) where 
  pure x = Compose (pure (pure x))
  Compose f <*> Compose g = Compose ( <*> (<$>) f <*> x)
-}

class Functor f => Monoidal f where 
  unit :: f ()
  tie :: f a -> f b -> f (a, b)


bind :: m a -> (a -> m b) -> m b 
bind = undefined 

join' :: Monad m => m (m a) -> m a
join' (outer) = outer >>= id

data MyMaybe a = MyNothing | MyJust a 

instance Functor MyMaybe where 
  fmap f MyNothing = MyNothing
  fmap f (MyJust x) = MyJust (f x)


instance Foldable MyMaybe where
  foldMap f MyNothing = mempty 
  foldMap f (MyJust x) = mempty <> (f x)

instance Traversable MyMaybe where 
  traverse _ MyNothing = pure MyNothing
  traverse (tFunc) (MyJust x) = MyJust <$> tFunc x


newtype MyLast a = MyLast {_getLast :: Maybe a}

instance Functor MyLast where 
  fmap f (MyLast Nothing) = MyLast Nothing 
  fmap f (MyLast (Just x)) = MyLast $ Just $ f x

instance Applicative MyLast where 
  pure a = MyLast Nothing
  f <*> (MyLast Nothing) = MyLast Nothing 
  (MyLast (Just f)) <*> (MyLast (Just x)) = MyLast (Just (f x))
  _ <*> _ = MyLast Nothing

instance Monoid (MyLast a) where 
  mempty = MyLast Nothing
  mappend (MyLast (Just a)) (MyLast Nothing) = MyLast (Just a) 
  mappend _ r = r


instance Monad MyLast where 
  return a = MyLast (Just a)
  MyLast Nothing >>= _ = MyLast Nothing
  MyLast (Just x) >>= f = f x


newtype MaybeT m a = MaybeT {runMaybe :: m (Maybe a)}


{-

instance Monad m => Monad (MaybeT m) where
  return a = MaybeT $ return $ Just a-}


flatmap :: (a -> [b]) -> [a] -> [b]
flatmap f i = myconcat $ Prelude.map f i

myconcat :: [[a]] -> [a]
myconcat l = case l of
    [[]] -> []
    [] -> []
    [] : t : t1 -> myconcat (t : t1)
    (h : t) : t1 -> h : (myconcat (t : t1))
