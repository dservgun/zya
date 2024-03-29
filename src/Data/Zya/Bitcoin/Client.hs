{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Bitcoin.Client where


import Control.Exception as E
import Control.Monad
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens(key, nth)
import Data.ByteString
import Data.ByteString.Lazy
import Data.Maybe
import Data.Map as Map
import Data.Monoid
import Data.Scientific
import Data.Text as T
import Data.Text.IO as TextIO
import Data.Text.Encoding (decodeUtf8)
import Data.Zya.Bitcoin.Common as BCommon
import Data.Zya.Bitcoin.Transaction as Transaction
import Data.Zya.Bitcoin.RawTransaction as RawTransaction
import Data.Zya.Utils.IPC
import Data.Zya.Utils.JsonRPC
import Data.Zya.Bitcoin.JsonRPC
import Network.HTTP.Client hiding(responseBody)
import Network.Socket
import Network.Wreq as NS -- no session
import System.IO


type Resp = Response (Map String Value)


doPost opts endPoint aRequest = do
  r <- asValue =<< NS.postWith opts endPoint aRequest :: IO (Response Value)
  let respBody = r ^? responseBody . key "result"
  if isNothing respBody then 
    return $ Just $ (String $ T.pack $ "Failed to return response : " <> (show aRequest))
  else 
    return respBody

getJSONRpcResponse :: [Char] -> [Char] -> UserName -> Password -> t -> IO (Maybe Value)
getJSONRpcResponse hostName' serviceName' (UserName userName') (Password password') _= do 
  let opts = defaults  
  handle
      (\e@(SomeException s) -> return $ Just $ String $ T.pack $ show e) 
      $ return Nothing

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
loadAddresses defaults' userName' password' addresses' =  
  mapM
      (\addr -> 
        uncurry getJSONRpcResponse defaults'
          userName'
          password'
          $ getAccountAddress (RequestId 1) addr) addresses'


transactionSummaries :: IO (Maybe (Result [TransactionSummary]))
transactionSummaries = 
  let 
    userName = UserName "loyakk_user1"
    password = Password "loyakk_password1"
  in
  handle (\a@(SomeException _) -> return $ Just $ Data.Aeson.Error (show a)) $ do 
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
          $ getListReceivedByAddress (RequestId 1) 6 True True
    let transactionSummaries' = 
                      --(fmap . fmap)
                      --(Prelude.filter(\x -> notEmptyAccountAddress x))
                      (fromJSON <$> resp)
    return transactionSummaries'


notEmptyAccountAddress :: TransactionSummary -> Bool
notEmptyAccountAddress summary = _account summary /= (AccountAddress "")

transactionDetails :: UserName -> Password -> Text -> IO (Maybe(Result RawTransaction)) 
transactionDetails userName password anId = do 
  resp <- uncurry getJSONRpcResponse someDefaults
            userName 
            password 
            $ getRawTransaction (RequestId 1) (anId)
  --System.IO.putStrLn $ show resp
  return $ fromJSON <$> resp

-- scary types.
transactionIds' :: IO
                    (Maybe (Result [(AccountAddress, BCommon.Address, [Text])]))
transactionIds' = 
    (fmap . fmap . fmap) 
      (Prelude.map (\x -> (_account x, _address x, _transactions x))) 
      transactionSummaries

transactionIds :: IO
                  (Maybe (Result [(AccountAddress, BCommon.Address, [Text])]))
transactionIds = transactionIds'





