module Zya.Utils.DistribUtils ( distribMain ) where


import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Static
import Data.ByteString
import Data.Text
import Data.Text.Encoding
import Language.Haskell.TH
import Network.Socket
import Network.Transport
import System.Environment


distribMain :: ([NodeId] -> Process ()) -> (RemoteTable -> RemoteTable) -> IO ()
distribMain master frtable = undefined

{-| The number of bits for the consistent hash, and the endpoint address -}
sha1 :: Int -> Text -> Int
sha1 = undefined

consistentHash :: NodeId -> []
consistentHash (NodeId (EndPointAddress anAddress)) = sha1 7 $ decodeUtf8 anAddress
