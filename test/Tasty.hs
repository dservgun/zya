{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Control.Concurrent.Async
import Control.Concurrent
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad.Reader
import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.Text as Text
import Data.Zya.Core.Writer
import Data.Zya.Core.Service
import Data.Zya.Core.Subscription
import Data.Zya.Core.ServiceTypes
import Data.Zya.Persistence.Persistence(DBType, persist)


testBackend :: IO Backend
testBackend = simpleBackend "localhost" "5000"

type Version = [Int]
isRecent :: (Eq a, Ord a) => a -> a -> Bool
isRecent = (<)

debugConnStrsqlite :: (DBType, ConnectionDetails)
debugConnStrsqlite = (RDBMS Sqlite, ConnectionDetails ":memory:")


debugConnStrPostgres :: (DBType, ConnectionDetails)
debugConnStrPostgres = 
    (RDBMS Postgresql, ConnectionDetails "host=localhost dbname=zya_debug user=zya_debug password=zya_debug port=5432")

debugConnStr = debugConnStrPostgres
newtype TServiceName = TServiceName {_unName :: String} deriving Show 
debugServiceName :: Text 
debugServiceName = 
    let s = TServiceName "testZYA" in
    Text.pack $ _unName s


createTopicTestCase :: Assertion
createTopicTestCase =  do 
  test <- testBackend 
  ta <- async $ cloudEntryPoint test (TopicAllocator, debugServiceName, fst debugConnStr, snd debugConnStr) 
  writer1 <- async $ cloudEntryPoint test (Writer, debugServiceName, fst debugConnStr, snd debugConnStr)
  writer2 <- async $ cloudEntryPoint test (Writer, debugServiceName, fst debugConnStr, snd debugConnStr)
  writer3 <- async $ cloudEntryPoint test (Writer, debugServiceName, fst debugConnStr, snd debugConnStr)  
  testWriter <- async $ cloudEntryPoint test (TestWriter, debugServiceName, fst debugConnStr,  snd debugConnStr)
  threadDelay (10 ^ 6 * 30) -- add a delay
  tb <- async $ cloudEntryPoint test (Terminator, debugServiceName, fst debugConnStr, snd debugConnStr)
  wait tb 

allTests :: TestTree
allTests = testGroup "Yet another zookeeper tests" [
  testGroup "HUnit tests" [
    testCase "createTopic allocator, shutdown and no exceptions.\n" createTopicTestCase
    ]
  ]

  

main :: IO () 
main = defaultMainWithIngredients defaultIngredients allTests