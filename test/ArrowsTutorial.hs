{-# LANGUAGE Arrows #-}

module ArrowsTest where 

import Control.Arrow
import Control.Monad 
import qualified Control.Category as Cat 
import Data.List
import Data.Maybe
import System.Random


newtype Circuit a b = Circuit {unCircuit :: a -> (Circuit a b, b)}


