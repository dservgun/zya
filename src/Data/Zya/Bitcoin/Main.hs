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
mainSearchBlock fileName aBlockId = do 
  searchTransactions 
    fileName
    $ BlockQuery (BlockHeight aBlockId, listAddresses)

main = do 
  [fileName] <- getArgs
  mainSearchBlock fileName 492828