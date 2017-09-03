{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Control.Concurrent.Async
import Control.Concurrent
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad.Reader
import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.Zya.Core.Writer
import Data.Zya.Core.Service
import Data.Zya.Core.Subscription
import Data.Zya.Core.ServiceTypes
import Data.Zya.Persistence.PersistZ
import Data.Zya.Persistence.Persistence(DBType, persist)


testBackend :: IO Backend
testBackend = simpleBackend "localhost" "5000"

type Version = [Int]
isRecent :: (Eq a, Ord a) => a -> a -> Bool
isRecent = (<)

-- change backend to using inmemory for tests.
createTopicTestCase :: Assertion
createTopicTestCase =  do 
  test <- testBackend 
  ta <- async $ cloudEntryPoint test (TopicAllocator, "testZYA") 
  threadDelay (10 ^ 6 * 3) -- add a delay
  tb <- async $ cloudEntryPoint test (Terminator, "testZYA")
  wait tb


allTests :: TestTree
allTests = testGroup "Yet another zookeeper tests" [
  testGroup "HUnit tests" [
    testCase "createTopic allocator, shutdown and no exceptions." createTopicTestCase
    , testCase "log createTopic, save to local database. No exceptions. Cant run as a test case" testMessagePersist]
  ]

  

main :: IO () 
main = defaultMainWithIngredients defaultIngredients allTests