{-# LANGUAGE InstanceSigs #-}
module SomeFunctions where  

import Control.Monad
data Tree a = Node a [Tree a] deriving(Show)
data MyMaybe a = MJust a | MNothing deriving (Show)

instance Functor MyMaybe where
    fmap f (MJust x) = MJust (f x)
    fmap f MNothing = MNothing


instance Functor Tree where 
    fmap f (Node a []) = Node (f a) []
    fmap f (Node a h)  = Node (f a) (map (\x -> fmap f x) h)

{-
instance Applicative Maybe where 
    pure = Just 
    f <*> (Just x) = Just (f x)
    f <*> Nothing = Nothing
-}

filterAsFold :: (a -> Bool) -> [a] -> [a]
filterAsFold f iList = 
  foldr (\ element accum -> 
          if (f element) 
          then 
            element : accum
          else
            accum) [] iList


newtype ZipList a = ZipList {un :: [a]}

instance Functor ZipList where 
  fmap f (ZipList a) = ZipList (map f a)
instance Applicative ZipList where 
  pure :: a -> ZipList a
  pure a = ZipList [a]
  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b 
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)

data W x = W x deriving Show 

instance Functor W where
  fmap f (W a) = W (f a)

instance Applicative W where 
  pure x = W x 
  (<*>) :: W (a -> b) -> W a -> W b 
  W f <*> W a = W (f a)
instance Monad W where 
  return x = W x
  W x >>= f = f x



testG :: Int -> W Int -> W Int 
testG x a@(W y) = do 
  y1 <- return x 
  return $ y1 + y

(>>==) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>==) Nothing f = Nothing 
(>>==) (Just a1) f = (f a1)


lbind :: [a] -> (a -> [a]) -> [a]
lbind [] f = [] 
lbind aList f = join $ fmap f aList