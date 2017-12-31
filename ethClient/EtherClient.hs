module Main where
import Data.Zya.Ethereum.Sockets.Client
import System.Log.Logger
import Data.Zya.Utils.Logger
import Data.Text as Text
import System.Environment(getArgs)
--main :: IO[([(SessionRequest, SessionResponse)], SessionState)]
-- TODO: Get parseopts.
main = blockBrowser


transactionQuery = do 
  setup DEBUG
  [txId] <- getArgs
  addFileHandler "debug.log" DEBUG
  addFileHandler "info.log" INFO
  queryTransactionTestMethod txId >>= \x -> infoMessage $ Text.pack $ show x


blockBrowser = do 
  setup DEBUG
  [blockId, range] <- getArgs
  addFileHandler "debug.log" DEBUG
  addFileHandler "info.log" INFO
  testMethod (read blockId, read range)