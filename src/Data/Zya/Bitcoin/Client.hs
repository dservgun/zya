{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Bitcoin.Client where


import Control.Exception
import Control.Exception as E
import Control.Monad
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens(key, nth)
import Data.ByteString
import Data.ByteString.Lazy
import Data.Map as Map
import Data.Monoid
import Data.Scientific
import Data.Text as T
import Data.Text.IO as TextIO
import Data.Text.Encoding (decodeUtf8)
import Data.Zya.Bitcoin.Common as BCommon
import Data.Zya.Bitcoin.BitcoinSession as BSession(Application, SessionStatus, SessionConfig(..))
import Data.Zya.Bitcoin.Transaction as Transaction
import Data.Zya.Bitcoin.RawTransaction as RawTransaction
import Data.Zya.Bitcoin.JsonRpc
import Data.Zya.Utils.IPC
import Data.Zya.Utils.JsonRPC
import Network.HTTP.Client hiding(responseBody)
import Network.Socket
import Network.Wreq
import System.IO



type Resp = Response (Map String Value)


doPost opts endPoint aRequest = do
  r <- asValue =<< postWith opts endPoint aRequest :: IO (Response Value)
  let respBody = r ^? responseBody . key "result"
  if respBody ==  Nothing then 
    return $ Just $ (String $ T.pack $ "Failed to return response : " <> (show aRequest))
  else 
    return respBody


getJSONRpcResponse hostName serviceName (UserName userName) (Password password) aRequest= do 
  let endPoint = "http://" <> userName <> ":" <> password <> "@" <> hostName <> ":" <> serviceName
  let opts = defaults  
  handle
      (\e@(SomeException s) -> return $ Just $ String $ T.pack $ show e) 
      $ doPost opts endPoint aRequest



someDefaults :: (String, String)
someDefaults = ("127.0.0.1", "8332")

addresses :: [AccountAddress]
addresses = AccountAddress 
            <$> 
            ["16R9ffu5BPcKHoy3dvZLQeghqHgHecEVWF", "1Pc2hje5qW62oajtZqayfs83bcSn2EiGbJ"
            , "15fnGtAJrqzct2fyE4SpGFQBKMnWWmv5fs"
            , "1DZzpDWEacmFVsw8KTRB6uqVxf7nAYdzmW"
            , "1C6nxuqAmytbXR162nC3Xw2TMsYkQJNw9C"]

-- Load all addresses in the client
loadAddresses :: (String, String) -> UserName -> Password -> [AccountAddress] -> IO [Maybe Value]
loadAddresses defaults userName password addresses =  
  mapM
      (\addr -> 
        getJSONRpcResponse 
          (fst someDefaults) 
          (snd someDefaults) 
          userName
          password
          $ getAccountAddress (RequestId "test") addr) addresses


transactionSummaries :: IO (Maybe (Result [TransactionSummary]))
transactionSummaries = 
  let 
    userName = UserName "loyakk_user1"
    password = Password "loyakk_password1"
  in
  handle (\a@(SomeException e) -> return $ Just $ Data.Aeson.Error (show a)) $ do 
    _ <- 
      loadAddresses someDefaults 
        (UserName "loyakk_user1")
        (Password "loyakk_password1") 
        addresses
    -- transaction summaries.
    resp <- 
        getJSONRpcResponse 
          (fst someDefaults) 
          (snd someDefaults) 
          userName
          password
          $ getListReceivedByAddress (RequestId "1") 6 True True
    let transactionSummaries = 
                      (fmap . fmap)
                      (Prelude.filter(\x -> notEmptyAccountAddress x))
                      (fromJSON <$> resp)
    return transactionSummaries


notEmptyAccountAddress :: TransactionSummary -> Bool
notEmptyAccountAddress summary = _account summary /= (AccountAddress "")

transactionDetails :: UserName -> Password -> Text -> IO (Maybe(Result RawTransaction)) 
transactionDetails userName password anId = do 
  resp <- getJSONRpcResponse 
            (fst someDefaults)
            (snd someDefaults)
            userName 
            password 
            $ getRawTransaction (RequestId "1") (anId)
  --System.IO.putStrLn $ show resp
  return $ fromJSON <$> resp

-- scary types.
transactionIds' = 
    (fmap . fmap . fmap) 
      (Prelude.map (\x -> (_account x, _address x, _transactions x))) 
      transactionSummaries

transactionIds = transactionIds'


transactionDetailsWithDefaults = 
    transactionDetails (UserName "loyakk_user1") (Password "loyakk_password1")




format :: (AccountAddress, BCommon.Address, [Maybe (Result RawTransaction)]) -> [Text]
format (account, address, transactions) = 
  Prelude.map(\l -> case l of 
                      Just (Success aTransaction) -> 
                        rawTransactionAsCSV account address aTransaction
                      _ -> 
                          T.pack $ "Error in transaction for " <> (show account)
                              <> " " <> (show address) <> ":" <> (show l)
              ) transactions

-- All transaction details with account information
f2 = do 
  x <- transactionIds
  case x of 
    Just y -> do      
      case y of 
        Success aList -> do 
          z1 <- mapM (\a@(x, addr, y) -> do 
                z <- mapM transactionDetailsWithDefaults y 
                return $ format (x, addr, z)) aList
          return $ Prelude.concat z1
    _ -> return $ []


writeToFile aFile = do 
  bracket (openFile aFile WriteMode) (hClose) $ \h -> do 
    TextIO.hPutStrLn h "Account, Address, Confirmation, Time, BlockTime, Amount, Address"
    f <- f2
    TextIO.hPutStrLn h (T.unlines f)



