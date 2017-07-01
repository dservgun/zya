{-# LANGUAGE TemplateHaskell #-}
module Zya.Utils.DistribUtils ( distribMain ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Static hiding (initRemoteTable)

import System.Environment
import Network.Socket hiding (shutdown)

import Language.Haskell.TH

{-| 
  
-}
distribMain :: ([NodeId] -> Process ()) -> (RemoteTable -> RemoteTable) -> IO ()
distribMain master frtable = undefined