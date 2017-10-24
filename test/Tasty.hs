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
import Data.Zya.Core.ComputeNodeService
import Data.Zya.Persistence.Persistence(DBType, persist)
import Test.Tasty
import Test.Tasty.HUnit as HU

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


createTopicTestCase :: Assertion
createTopicTestCase =  do
  test <- testBackend
  --ta <- async $ cloudEntryPoint test (TopicAllocator, debugServiceName, fst debugConnStr, snd debugConnStr, Nothing)
  let nWriters = 1
  let messages = 100 -- Messages to be published.
  testWebService <- async $ cloudEntryPoint test (WebServer, debugServiceName, fst debugConnStr, snd debugConnStr, Just messages, 30000)
  query1 <- async $ cloudEntryPoint test (QueryService, debugServiceName, fst debugConnStr, snd debugConnStr, Just (nWriters * messages), -1)
  query1 <- async $ cloudEntryPoint test (QueryService, debugServiceName, fst debugConnStr, snd debugConnStr, Just (nWriters * messages), -1)

  writers <- forM [1..nWriters] $ \_ -> do
                async $ cloudEntryPoint test (Writer, debugServiceName, fst debugConnStr, snd debugConnStr, Just messages, -1)
  testWriter <- async $ cloudEntryPoint test (TestWriter, debugServiceName, fst debugConnStr,  snd debugConnStr, Just messages, -1)
  -- Run for 30 seconds and quit.


  threadDelay (fromIntegral $ (10 ^ 6 * 30))


allTests :: TestTree
allTests = testGroup "Yet another zookeeper tests" [
  testGroup "HUnit tests" [
    testCase "createTopic allocator, shutdown and no exceptions.\n" createTopicTestCase
    ]
  ]




main :: IO ()
main = defaultMainWithIngredients defaultIngredients allTests