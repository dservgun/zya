{-# LANGUAGE InstanceSigs #-}
module BinaryTrees where 
import Control.Comonad
import Data.Monoid
import Prelude
import Test.QuickCheck
import Data.Foldable

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) 
          | Leaf a 
          deriving (Show)


instance Functor BinaryTree where 
  fmap f a = 
    case a of 
      Node a1 l r -> Node (f a1) (fmap f l) (fmap f r)
      Leaf a2 -> Leaf (f a2)


instance Foldable BinaryTree where 
  foldMap foldFunction a = 
    case a of
      Node a1 l r -> (foldMap foldFunction l) <> foldFunction a1 <> (foldMap foldFunction r) 
      Leaf a2 -> foldFunction a2


sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL aList = foldr (\ele acc -> combine acc ele) (pure []) aList

combine :: Applicative f => f [a] -> f a -> f [a]
combine acc ele = add <$> ele <*> acc

add :: a -> [a] -> [a]
add h t = h : t


data MyStream a = Cons a (MyStream a)

instance Functor MyStream where 
  fmap f (Cons a1 b) = Cons (f a1) (fmap f b)


instance Comonad MyStream where
  extract :: MyStream a -> a 
  extract (Cons x _) = x
  duplicate :: MyStream a -> MyStream (MyStream a)
  duplicate a1@(Cons _ b) = Cons a1 (duplicate b)
  extend :: (MyStream a -> b) -> MyStream a -> MyStream b 
  extend g s@(Cons x xs) = Cons (g s) (extend g xs)


data U x = U [x] x [x]

instance Functor U where 
  fmap f (U left center right) = U (map f left) (f center) (map f right)


data RoseTree a = RNode a [RoseTree a]

instance Functor RoseTree where
  fmap f (RNode a children) = RNode (f a) $ map (\c -> fmap f c) children

data AT a = L a | B (AT a) (AT a) 

instance Functor AT where
  fmap f (L a) = L (f a) 
  fmap f (B (t1) (t2)) = B (fmap f t1) (fmap f t2)

instance Applicative AT where 
  pure a = L a
  L f <*> L a = L (f a)
  L f <*> (B a1 a2) = B (f <$> a1) (f <$> a2)
  (B f1 f2) <*> (B a1 a2) = B( f1 <*> a1) (f2 <*> a2)
  (B f1 f2) <*> (L a) = B (f1 <*> (L a)) (f2 <*> (L a ))

instance Monad AT where 
  return a = L a
  t >>= k = 
    case t of 
      L x -> k x 
      B a1 a2 -> B (a1 >>= k) (a2 >>= k)


fructify :: AT a -> AT a 
fructify (L a)  = B (L a) (L a)
fructify (B a b) = B (fructify a) (fructify b)


appBind :: (Monad f, Applicative f) => f (a -> b) -> f a -> f b
appBind f1 container = f1 >>= \f -> f <$> container


