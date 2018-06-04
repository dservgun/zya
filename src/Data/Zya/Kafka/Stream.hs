module Data.Zya.Kafka.Stream
(
  KafkaStreamMessage
  , KafkaStreamResult
  , computeStream

) where 
import Haskakafka
import Control.Concurrent.STM(TBQueue(..), STM(..), readTBQueue, writeTBQueue)


data KafkaStreamMessage a = KafkaStreamMessage {
  _message :: KafkaMessage
  , _map :: KafkaMessage -> a
}

data KafkaStreamResult a b = KafkaStreamResult {
  _accumulator :: b 
  , _incr :: a -> b -> b
}

-- KafkaStreamMessage is the last message processed. 
data KafkaStream a b = 
  KafkaStream {
  messagePair :: TBQueue (KafkaStreamMessage a, KafkaStreamResult a b)
}


foldStream :: KafkaStreamMessage a -> KafkaStreamResult a b -> KafkaStreamResult a b 
foldStream 
  (KafkaStreamMessage m2 mapF)
  r@(KafkaStreamResult acc accumulator) = 
      KafkaStreamResult (accumulator (mapF m2) acc) accumulator

computeStream :: (KafkaStreamMessage a -> KafkaStreamResult a b -> KafkaStreamResult a b) -> 
                  KafkaStream a b -> STM ()
computeStream foldFunction (KafkaStream mQ) = do 
  (message, prevResult) <- readTBQueue mQ 
  let result = foldStream message prevResult
  writeTBQueue mQ (message, result)
  return ()



