module Main where 
import Data.Zya.Bitcoin.BitcoinSession
import Data.Text
import Data.Traversable
import System.Environment(getArgs)


main = do
  [fileName] <- getArgs
  generalLedgerApplication fileName
