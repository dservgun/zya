module Data.Zya.Kafka.Util where 

import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as L8

int16ToWord8Array :: Int -> [Word8] 
int16ToWord8Array aNumber = 
  [
    fromIntegral (aNumber `shiftR` 8)
    , fromIntegral aNumber

  ]
-- Big endian
int32ToWord8Array :: Int -> [Word8]
int32ToWord8Array aNumber = 
  [
    fromIntegral  (aNumber `shiftR` 24)
    , fromIntegral (aNumber `shiftR` 16)
    , fromIntegral (aNumber `shiftR` 8)
    , fromIntegral (aNumber) 
  ]


int64ToWordArray :: Word64 -> [Word8] 
int64ToWordArray aNumber = 
  [
    fromIntegral (aNumber `shiftR` 56) 
    , fromIntegral (aNumber `shiftR` 48)
    , fromIntegral (aNumber `shiftR` 40)
    , fromIntegral (aNumber `shiftR` 32)
    , fromIntegral (aNumber `shiftR` 24)
    , fromIntegral (aNumber `shiftR` 16) 
    , fromIntegral (aNumber `shiftR` 8)
    , fromIntegral aNumber

  ]



wordArrayToInt32 :: [Word8] -> Int 
wordArrayToInt32 anArray = Prelude.foldl accum 0 anArray
  where accum a o = (a `shiftL` 8) .|. (fromIntegral o)