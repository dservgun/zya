{-# LANGUAGE OverloadedStrings #-}

module Data.Zya.Client.SimpleClient where 

import        Control.Concurrent(forkIO)
import        Control.Monad (forever, unless)
import        Control.Monad.Trans(liftIO)
import        Network.Socket (withSocketsDo)
import qualified  Data.Text as T 
import qualified  Data.Text.IO as T 
import qualified  Network.WebSockets as WS
import Data.Monoid
import Data.Aeson
import Data.Text.Encoding(encodeUtf8)
import System.Environment(getArgs)
parseJson :: T.Text -> Either String Value 
parseJson = eitherDecodeStrict . encodeUtf8 
app :: WS.ClientApp () 
app conn = do 
  putStrLn "Connected!"
  _ <- forkIO $ forever $ do 
          msg <- WS.receiveData conn 
          liftIO $ putStrLn $ show msg
          liftIO $ putStrLn $ show $ parseJson msg
  let loop = do 
              line <- T.getLine
              unless (T.null line) $ putStrLn $ show $ ("receved " :: T.Text) <> line
  loop
  WS.sendClose conn("Bye!" :: T.Text)


mainApp :: IO () 
mainApp = do 
  [portNumber] <- map read <$> getArgs
  withSocketsDo $ WS.runClient "localhost" portNumber "/" app