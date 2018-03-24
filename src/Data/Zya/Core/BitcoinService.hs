{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.BitcoinService(
  -- * The cloud service used to test readers and writers.
  
  ) where



import Control.Concurrent
import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Reader

import Data.Monoid((<>))
import Data.Text(pack, unpack, Text)
import Data.Time(getCurrentTime)
import Data.UUID.V1(nextUUID)
import Data.Zya.Core.LocalMessageHandlingStrategy(runMessageWriter)
import Data.Zya.Core.Service
import Data.Zya.Utils.Logger as Logger
import Data.Zya.Utils.ComponentDetails(ComponentName(..))

data BitcoinService = BitcoinService deriving (Show) 

instance ComponentName BitcoinService where 
  componentName BitcoinService = "Data.Zya.Core.BitcoinService"

