{-# LANGUAGE OverloadedStrings #-}
module Main where 
import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.Zya.Core.Service
import Data.Zya.Core.Subscription



testBackend = simpleBackend "localhost" "5000"
createTopicTestCase :: Assertion
createTopicTestCase =  do 
  test <- testBackend 
  server <- newServerIO
  cloudEntryPoint test (TopicAllocator, "testZYA") server


allTests :: TestTree
allTests = testGroup "Yet another zookeeper tests" [
  testGroup "HUnit tests" [testCase "createTopic" createTopicTestCase]
  ]
main :: IO () 
main = defaultMain allTests