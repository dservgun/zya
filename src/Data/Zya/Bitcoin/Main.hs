{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Data.Zya.Bitcoin.Block
import Data.Zya.Bitcoin.Common
import Data.Zya.Bitcoin.BitcoinSession
import Data.Text
import Data.Traversable
import System.Environment(getArgs)


mainGL = do
  [fileName] <- getArgs
  generalLedgerApplication fileName

mainSearchBlock fileName aBlockId = do 
  searchTransactions 
    fileName
    $ BlockQuery (BlockHeight aBlockId, 
        [
          Address $ pack "1L8zVjitQnCkweTnMY5ioAvvEnMksWvGxU"
          , Address $ pack "1L8zVjitQnCkweTnMY5ioAvvEnMksWvGxU"
        ])

main = do 
  [fileName] <- getArgs
  mainSearchBlock fileName 504347