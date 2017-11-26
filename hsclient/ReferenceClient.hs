module Main where 
import Data.Zya.Client.SimpleClient as SimpleClient
main :: IO () 
main = do 
    _ <- putStrLn("Welcome to zya.")
    SimpleClient.mainApp