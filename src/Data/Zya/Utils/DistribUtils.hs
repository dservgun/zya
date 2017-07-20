module Zya.Utils.DistribUtils ( distribMain ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Static hiding (initRemoteTable)
import Network.Transport
import Data.Text
import Data.Text.Encoding
import Data.ByteString
import System.Environment
import Network.Socket hiding (shutdown)

import Language.Haskell.TH

{-| 
  
-}
distribMain :: ([NodeId] -> Process ()) -> (RemoteTable -> RemoteTable) -> IO ()
distribMain master frtable = undefined

{-| The number of bits for the consistent hash, and the endpoint address -}
sha1 :: Int -> Text -> Int
sha1 = undefined

consistentHash :: NodeId -> []
consistentHash (NodeId (EndPointAddress anAddress)) = sha1 7 $ decodeUtf8 anAddress 
