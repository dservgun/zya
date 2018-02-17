{-# LANGUAGE OverloadedStrings #-}
module Main where 


import Data.Text
import Data.Traversable
import Data.Zya.Bitcoin.BitcoinSession
import Data.Zya.Bitcoin.Block
import Data.Zya.Bitcoin.Common
import Data.Zya.Utils.Logger(setup, debugMessage, infoMessage, errorMessage, addFileHandler)
import System.Environment(getArgs)
import System.Log.Logger
import Data.Zya.Bitcoin.CommandLineParser(mainCLI)

setupLogging = do 
  setup DEBUG
  addFileHandler "btc.debug.log" DEBUG
  addFileHandler "btc.info.log" INFO 


{-- 
"16R9ffu5BPcKHoy3dvZLQeghqHgHecEVWF"
"1Pc2hje5qW62oajtZqayfs83bcSn2EiGbJ"
"15fnGtAJrqzct2fyE4SpGFQBKMnWWmv5fs"
"1DZzpDWEacmFVsw8KTRB6uqVxf7nAYdzmW"
"1C6nxuqAmytbXR162nC3Xw2TMsYkQJNw9C"
"1L8zVjitQnCkweTnMY5ioAvvEnMksWvGxU"
"1DTXpdZrUxniyiB9kggCiMSiUtVkzXnGY6"
"19YkAYbr5gLAvgZ7RgeUrjpUonWAU3VVwL"
--}
listAddresses :: [Address]
listAddresses = 
  Prelude.map (Address . pack) $ 
    [
    "16R9ffu5BPcKHoy3dvZLQeghqHgHecEVWF"
    , "14nyFkVJHHBGR7bYioWn6BxjppMjT3KA1r"
    , "18NY8DSvhJjv5TaC2dDSXMHNi8f8ma3LPG"
    , "19YkAYbr5gLAvgZ7RgeUrjpUonWAU3VVwL"
    , "1DTXpdZrUxniyiB9kggCiMSiUtVkzXnGY6"
    , "1KxDrjD1EMcYvKq6VTBp7Fb2h8vMww12ts"
    ,"15fnGtAJrqzct2fyE4SpGFQBKMnWWmv5fs"
    ,"1C6nxuqAmytbXR162nC3Xw2TMsYkQJNw9C"
    ,"1DZzpDWEacmFVsw8KTRB6uqVxf7nAYdzmW"
    ,"1L8zVjitQnCkweTnMY5ioAvvEnMksWvGxU"
    ,"1Pc2hje5qW62oajtZqayfs83bcSn2EiGbJ"
    ]
mainSearchBlock fileName blockId = do 
  searchTransactions 
    fileName
    $ BlockQuery (BlockHeight blockId, listAddresses)
generateAddressesM = do
  [numAdd] <- getArgs 
  generateAddresses $ (read numAdd)
main1 = do 
  [fileName] <- getArgs
  setupLogging  
  let blocksToTravel = 10
  mapM (\ block -> mainSearchBlock fileName block) $ 
      Prelude.take blocksToTravel $ Prelude.take 2 [508707 ..]


main = do
  setupLogging 
  mainCLI


