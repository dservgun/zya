module Data.Zya.Kafka.Util where 

import Data.Word
import Data.Bits


int16ToWord8Array :: Word16 -> [Word8] 
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

