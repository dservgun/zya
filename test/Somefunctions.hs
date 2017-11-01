{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module SomeFunctions where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Concurrent.STM
import Data.Text as Text hiding(foldl, map, foldr, zipWith)

import Data.Set as Set hiding(foldl, map, foldr)
data Tree a = Node a [Tree a] deriving(Show)
data MyMaybe a = MJust a | MNothing deriving (Show)

instance Functor MyMaybe where
    fmap f (MJust x) = MJust (f x)
    fmap f MNothing = MNothing


instance Functor Tree where
    fmap f (Node a []) = Node (f a) []
    fmap f (Node a h)  = Node (f a) (map (\x -> fmap f x) h)


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

type Rank = Int
data Heap a =
    Empty | Heap Rank a (Heap a) (Heap a) deriving Show


rank :: Heap a -> Int
rank Empty = 0
rank (Heap x _ _ _) = x

-- The property below about minimum of the ranks of
-- any two nodes follows from the observation (reference intertubes)
-- than the rank of a node (ie, the path to an empty node) is
-- 1 + (min (rank l, rank r))
makeElem :: a -> Heap a -> Heap a -> Heap a
makeElem x a b =
    if rank a >= rank b then
      Heap (rank b + 1) x a b
    else
      Heap (rank a + 1) x b a
hMerge :: (Ord a) => Heap a -> Heap a -> Heap a
hMerge Empty Empty = Empty
hMerge h Empty = h
hMerge Empty h = h
hMerge (h1@(Heap _ x a1 a2)) (h2@(Heap _ y b1 b2)) =
    if x <= y then
        makeElem x a1 $ hMerge a2 h1
    else
        makeElem y b1 $ hMerge b2 h1

findMin :: Heap a -> Maybe a
findMin Empty = Nothing
findMin (Heap _ x _ _) = Just x

heapFromListS :: Ord a => [a] -> Heap a
heapFromListS l =
    foldl
      (\ acc ele -> hMerge acc (makeElem ele Empty Empty))
      Empty l


heapFromList :: Ord a => [a] -> Heap a -> Heap a
heapFromList [] heap = heap
heapFromList (h : t) heap = heapFromList t (hMerge heap $ Heap 1 h Empty Empty)

heapFromListP :: Ord a => [a] -> Heap a -> Heap a
heapFromListP [] heap = heap
heapFromListP [a] heap = hMerge (heap) (Heap 1 a Empty Empty)
heapFromListP (a1:a2:t) heap =
  heapFromListP t (hMerge heap
    (hMerge (Heap 1 a1 Empty Empty) (Heap 1 a2 Empty Empty)))

type Idn = String
data Expr =
    Num Int
  | Var Idn
  | Lam(Idn, Expr)
  | App(Expr, Expr)
  | Sub(Expr, Idn, Expr) deriving Show


freeVariables :: Expr -> Set Idn
freeVariables (Num _) = Set.empty
freeVariables(Var x) = Set.insert (x) (Set.empty)
freeVariables(Lam(idn, expr)) = Set.delete idn $ freeVariables(expr)
freeVariables(App(m, n)) =
    Set.union
        (freeVariables(m))
        (freeVariables(n))
freeVariables(Sub(m, x, n)) =
    Set.union
      (Set.delete x (freeVariables m))
      (freeVariables n)


f1 :: Int -> Int -> Bool
f1 a b = a + b > a
f2 :: Int -> Int
f2 a = a


type Process = IO
newtype AvailableServerState a =
    AServerState {
      runAs :: StateT(Maybe ProcessId) (ReaderT (ServiceProfile, FairnessStrategy) Process) a
      } deriving(
          Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadState (Maybe ProcessId)
        , MonadReader (ServiceProfile, FairnessStrategy)
        )

type ProcessId = Int
type ServiceProfile = Int
type Server = Int
type PMessage = String
type FairnessStrategy = Int
findAvailableService :: Server -> ServiceProfile -> FairnessStrategy -> STM(Maybe ProcessId)
findAvailableService = undefined


sendMessage :: PMessage -> Server -> AvailableServerState ()
sendMessage p s = do
  void $ liftIO $ atomically $ findAvailableService s 1 2


newtype MessageResponse = MessageResponse{_unResponse :: Text} deriving(Show)
newtype MessageRequest = MessageRequest {_unRequest :: Text} deriving (Show)

parseMessageResponse :: MessageResponse -> Text
parseMessageResponse = undefined

parseMessageRequest :: MessageRequest -> Text
parseMessageRequest = undefined
type family JSONParser a where
  JSONParser MessageResponse = MessageResponse
  JSONParser MessageRequest = MessageResponse


data family XList a
data instance XList char = XCons !Char !(XList Char) | Nil
data instance XList () = XListunit !Int