{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Ethereum.Internal.Bookeeping.ReconTransaction
  (
    reconTransactions, makeRecon, ReconTransaction, address, intentionAmount, toCSV
  ) where

import Control.Applicative
import Control.Exception (bracket, handle, SomeException(..), catch)
import Control.Monad (forever, unless)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Trans (liftIO)
import Data.Text.IO as TextIO
import Data.Monoid
import Data.Zya.Utils.JsonRPC
import Data.Zya.Ethereum.Internal.Types.Common
import Data.Zya.Ethereum.Internal.Types.RPCRequest
import Data.Zya.Ethereum.Internal.Types.RPCResponse
import Data.Zya.Ethereum.Internal.Types.Transaction
import Data.Zya.Ethereum.Sockets.GethApplication
import Data.Zya.Utils.FileUtil (readInputLines)
import Data.Zya.Utils.Logger (debugMessage, infoMessage, errorMessage)
import Data.Zya.Utils.IPC (sendMessageWithSockets, domainSocket, closeHandle)
import Network.Socket (Socket, close)
import Network.Socket.ByteString
import qualified Data.Text as T
import qualified Data.Text.IO as T 
import System.IO
import Text.Printf
import qualified Data.ByteString.Lazy as BL 
import Data.Csv 
import qualified Data.Vector as V 




data ReconTransaction = ReconTransaction {
  address :: Integer
  , intentionAmount :: Double 
} deriving (Show, Eq)

makeRecon :: [T.Text] -> ReconTransaction
makeRecon = undefined

parseDouble :: String -> Double
parseDouble "NULL" = 0.0
parseDouble x = read x

readReconTransactions :: FilePath -> IO [ReconTransaction]
readReconTransactions aFilePath = do
  lines <- BL.readFile aFilePath
  case decode HasHeader lines of 
    Left err -> System.IO.putStrLn (show err) >> return [] 
    Right v -> do 
        vector <-       
            V.forM (v) $ 
              \(email :: String, rAddress :: String, amount :: String, actual :: String, ver :: String, verDate :: String, verBy :: String, createdTs :: String) -> 
                return $ ReconTransaction (read rAddress) (parseDouble amount)
        return $ V.toList vector

reconTransactions :: FilePath -> IO [ReconTransaction]
reconTransactions reconFile = readReconTransactions reconFile 

toCSV :: ReconTransaction -> T.Text 
toCSV (ReconTransaction address amount) = T.pack $ (show address) <> "," <> (show amount)