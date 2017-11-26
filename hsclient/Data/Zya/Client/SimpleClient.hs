{-# LANGUAGE OverloadedStrings #-}

module Data.Zya.Client.SimpleClient where 

import 				Control.Concurrent(forkIO)
import				Control.Monad (forever, unless)
import				Control.Monad.Trans(liftIO)
import				Network.Socket (withSocketsDo)
import qualified	Data.Text as T 
import qualified 	Data.Text.IO as T 
import qualified 	Network.WebSockets as WS
import Data.Monoid


app :: WS.ClientApp () 
app conn = do 
	putStrLn "Connected!"
	_ <- forkIO $ forever $ do 
		msg <- WS.receiveData conn 
		liftIO $ T.putStrLn msg 

	let loop = do 
		line <- T.getLine
		unless (T.null line) $ putStrLn $ show $ ("receved " :: T.Text) <> line
	loop
	WS.sendClose conn("Bye!" :: T.Text)


mainApp :: IO () 
mainApp = withSocketsDo $ WS.runClient "localhost" 30001 "/" app