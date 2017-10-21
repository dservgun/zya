{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad.Reader
import Data.Text as Text
import Data.Zya.Core.Service
import Data.Zya.Core.Subscription
import Data.Zya.Core.WebServerService
import Data.Zya.Persistence.Persistence(DBType, persist)


{-- |
 A reference test client. Messages are generated by the test writer. The webserver publishes
 the stream.
 The system snapshot could comprise of
  . General health of the system: approximate number of messages processed in the last 1 minute for example.
  . Endpoint distribution: how ports are distributed for the webserver, so clients can connect to view the state.
--}
main :: IO ()
main = startServices



startServices =  do
  test <- testBackend
  --ta <- async $ cloudEntryPoint test (TopicAllocator, debugServiceName, fst debugConnStr, snd debugConnStr, Nothing)
  let nWriters = 6
  let messages = 1000 -- Messages to be published.
  testWebService <- async $ cloudEntryPoint test (WebServer, debugServiceName, fst debugConnStr, snd debugConnStr, Just messages, 30000)
  query1 <- async $ cloudEntryPoint test (QueryService, debugServiceName, fst debugConnStr, snd debugConnStr, Just (nWriters * messages), -1)
  query1 <- async $ cloudEntryPoint test (QueryService, debugServiceName, fst debugConnStr, snd debugConnStr, Just (nWriters * messages), -1)

  writers <- forM [1..nWriters] $ \_ -> do
                async $ cloudEntryPoint test (Writer, debugServiceName, fst debugConnStr, snd debugConnStr, Just messages, -1)
  testWriter <- async $ cloudEntryPoint test (TestWriter, debugServiceName, fst debugConnStr,  snd debugConnStr, Just messages, -1)

  wait testWebService




testBackend :: IO Backend
testBackend = simpleBackend "localhost" "5000"

type Version = [Int]
isRecent :: (Ord a) => a -> a -> Bool
isRecent = (<)

debugConnStrSqlite :: (DBType, ConnectionDetails)
debugConnStrSqlite = (RDBMS Sqlite, ConnectionDetails ":memory:")


debugConnStrPostgres :: (DBType, ConnectionDetails)
debugConnStrPostgres =
    (RDBMS Postgresql, ConnectionDetails "host=localhost dbname=zya_debug user=zya_debug password=zya_debug port=5432")

--debugConnStr = debugConnStrPostgres
debugConnStr = debugConnStrSqlite
newtype TServiceName = TServiceName {_unName :: String} deriving Show
debugServiceName :: Text
debugServiceName =
    let s = TServiceName "testZYA" in
    Text.pack $ _unName s
