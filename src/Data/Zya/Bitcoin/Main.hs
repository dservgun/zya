module Main where 
import Data.Zya.Bitcoin.BitcoinSession


main :: IO () 
main = do
  generalLedgerApplication addresses 
  return ()