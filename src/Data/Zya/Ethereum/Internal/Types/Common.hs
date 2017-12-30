{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Ethereum.Internal.Types.Common (
  Transaction(from, to)
  , BlockQuantity
  , BlockByHash(..)
  , BlockQuantity(..)
  , EthParameter(..)
  ) where
import Data.Aeson
import GHC.Generics
import Data.Zya.Ethereum.Internal.Types.Transaction 
import Data.Text
import Text.Printf
import Test.QuickCheck
type Quantity = Integer

data BlockQuantity = Earliest | Latest | Pending | BlockId Integer

instance Arbitrary BlockQuantity where 
  arbitrary = do 
    blockId <- arbitrary
    elements [Earliest, Latest, Pending, BlockId blockId]
class EthParameter a m where 
  toEthParam :: (Functor m) => a -> Bool -> m Value

instance EthParameter BlockQuantity [] where 
  toEthParam a details = 
    case a of 
      Earliest -> 
        [String "earliest", Bool details]
      Latest -> 
        [String "latest", Bool details]
      Pending -> 
        [String "pending" , Bool details]
      BlockId anInteger ->  [String (blockId anInteger), Bool details]
      where
        blockId :: Integer -> Text
        blockId anId = pack (Text.Printf.printf "0x%x" anId)

instance FromJSON BlockQuantity where 
  parseJSON = withText "BlockQuantity" $ \v -> 
        return 
          $ case (unpack v) of 
            "earliest" -> Earliest
            "latest" -> Latest
            "pending" -> Pending
            n@(_) -> BlockId $ read n

instance ToJSON BlockQuantity where 
  toJSON aBlockQuantity = 
    case aBlockQuantity of 
        Earliest -> String "earliest"
        Latest -> String "latest"
        Pending -> String "pending" 
        BlockId anInt -> String . pack $ printf "0x%x" anInt
instance Show BlockQuantity where 
  show a = 
    case a of
      Earliest -> "earliest" 
      Latest -> "latest"
      Pending -> "pending" 
      BlockId anId -> printf "%d" anId


data BlockPending = BlockPending deriving(Show)

data BlockByHash = BlockByHash {
  -- | the block number or null if pending.
  number :: BlockQuantity
  -- | Hash of the block
  , hash :: Integer 
  -- | Parent hash 
  , parentHash :: Integer
  -- | 8 byte hash of the generated proof of work.
  , nonce :: Integer
  -- | SHA3 of the uncle blocks.
  , sha3Uncles :: Integer 
  -- | Bloom filter logs of the block.
  , logsBloom :: Integer
  -- | Root of the transaction trie of the block.
  , transactionsRoot :: Integer
  -- | Root of the final state trie of the block.
  , stateRoot :: Integer 
  -- | Receipts root of the recepits of the trie of the block.  
  , receiptsRoot :: Integer
  -- | Address of the beneficiary to whom mining rewards
  -- | are given. 
  , miner :: Integer
  -- | Difficulty of this block.  
  , difficulty :: Integer
  -- | Total difficulty of this block.
  , totalDifficulty :: Integer
  -- | Extra data associated with this block.
  , extraData :: Integer
  -- | Size of this block in bytes. 
  -- Should the size have anothe property to check if the block was modified anywhere?
  , size :: Integer
  -- | Gas limit allowed for this block.
  , gasLimit :: Integer
  -- | Total used gas by all the transactions in this block.
  , gasUsed :: Integer
  -- | timestamp
  , timestamp :: Integer
  -- | Transactions array
  , transactions :: [Transaction]
  -- | Array of uncle hashes.
  , uncles :: [Integer]
} deriving (Generic, Show)

instance FromJSON BlockByHash where 
  parseJSON = withObject "BlockByHash" $ \w -> do
    v <- w .: "result"
    number <- v .: "number" 
    hash <- v .: "hash" 
    parentHash <- v .: "parentHash"
    nonce <- v .: "nonce"
    sha3Uncles <- v .: "sha3Uncles" 
    logsBloom <- v .: "logsBloom" 
    transactionsRoot <- v .: "transactionsRoot"
    stateRoot <- v .: "stateRoot" 
    receiptsRoot <- v .: "receiptsRoot"
    miner <- v .: "miner" 
    difficulty <- v .: "difficulty" 
    totalDifficulty <- v .: "totalDifficulty" 
    extraData <- v .: "extraData"
    size <- v .: "size" 
    gasLimit <- v .: "gasLimit"
    gasUsed <- v .: "gasUsed" 
    timestamp <- v .: "timestamp" 
    transactions <- v .: "transactions"
    uncles <- v .: "uncles"
    return $ BlockByHash 
              (number)
              (read hash) (read parentHash)
              (read nonce) (read sha3Uncles)
              (read logsBloom) (read transactionsRoot) 
              (read stateRoot) (read receiptsRoot) 
              (read miner) (read difficulty) (read totalDifficulty) 
              (read extraData) (read size) (read gasLimit) (read gasUsed)
              (read timestamp) transactions uncles

instance ToJSON BlockByHash where 
  toJSON (BlockByHash number hash parentHash nonce sha3Uncles 
            logsBloom transactionsRoot stateRoot
            receiptsRoot miner difficulty totalDifficulty
            extraData size gasLimit gasUsed timestamp transactions uncles) = 
      object [
          "number" .= number
          , "hash" .= (String . pack $ printf "0x%x" hash)
          , "parentHash" .= (String . pack $ printf "0x%x" parentHash)
          , "nonce" .= (String . pack $ printf "0x%x" nonce)
          , "sha3Uncles" .= (String . pack $ printf "0x%x" sha3Uncles)
          , "logsBloom" .= (String . pack $ printf "0x%x" logsBloom)
          , "transactionsRoot" .= (String . pack $ printf "0x%x" transactionsRoot)
          , "stateRoot" .= (String . pack $ printf "0x%x" stateRoot)
          , "receiptsRoot" .= (String . pack $ printf "0x%x" receiptsRoot) 
          , "miner" .= (String . pack $ printf "0x%x" miner)
          , "difficulty" .= (String . pack $ printf "0x%x" difficulty) 
          , "totalDifficulty" .= (String . pack $ printf "0x%x" totalDifficulty) 
          , "extraData" .= (String . pack $ printf "0x%x" extraData)
          , "size" .= (String . pack $ printf "0x%x" size)
          , "gasLimit" .= (String . pack $ printf "0x%x" gasLimit)
          , "gasUsed" .= (String . pack $ printf "0x%x" gasUsed)
          , "timestamp" .= (String . pack $ printf "0x%x" timestamp)
          , "transactions" .= transactions
      ] 

