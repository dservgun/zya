{-# LANGUAGE OverloadedStrings #-}
module Main where 
import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.Zya.Core.Service
import Data.Zya.Core.Subscription
import Control.Concurrent.Async


testBackend = simpleBackend "localhost" "5000"
createTopicTestCase :: Assertion
createTopicTestCase =  do 
  test <- testBackend 
  ta <- async $ cloudEntryPoint test (TopicAllocator, "testZYA") 
  tb <- async $ cloudEntryPoint test (Terminator, "testZYA")
  wait tb

allTests :: TestTree
allTests = testGroup "Yet another zookeeper tests" [
  testGroup "HUnit tests" [testCase "createTopic allocator, shutdown and no exceptions." createTopicTestCase]
  ]
main :: IO () 
main = defaultMainWithIngredients defaultIngredients allTests