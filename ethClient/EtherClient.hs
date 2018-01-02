module Main where
import Data.Zya.Ethereum.Sockets.Client
import System.Log.Logger
import Data.Zya.Utils.Logger
import Data.Text as Text
import System.Environment(getArgs)
import Options.Applicative
import Data.Semigroup((<>))
--main :: IO[([(SessionRequest, SessionResponse)], SessionState)]
-- TODO: Get parseopts.


data EtherClientCommand = 
  TransactionQuery {ipcPath :: FilePath, address :: String, transactionId :: Text}
    | BlockBrowser {ipcPath :: FilePath, address :: String, blockId :: Integer, numberOfBlocks :: Integer, defaultBlockChunks :: Integer}
  deriving(Show)  



blockIdParser :: Parser Integer 
blockIdParser = 
  read <$> 
    strOption
      (long "blockId"
        <> metavar "BLOCK ID"
        <> help "Block id to start scanning from")

numberOfBlocksParser :: Parser Integer 
numberOfBlocksParser = 
  read <$> 
    strOption
      (long "range"
        <> metavar "NUMBEROFBLOCKS" 
        <> help "The number of blocks we want to scan over.")

defaultBlockChunksParser :: Parser Integer 
defaultBlockChunksParser = 
  read <$> 
    strOption
    (
      long "chunkSize"
      <> metavar "BLOCKCHUNKS"
      <> help "The number of blocks to query at any given time usually a number between 1 to 100"
    )

ipcPathParser :: Parser FilePath
ipcPathParser = 
  strOption
    (long "ipcPath"
     <> metavar "IPCPATH"
     <> help "The ipc path for the eth node") 

addressParser :: Parser String 
addressParser = 
  strOption
    (
      long "address" 
      <> metavar "ADDRESS"
      <> help ("The hash for the address")
    )

transactionIdParser :: Parser Text 
transactionIdParser = 
  Text.pack
  <$> strOption 
      (long "transactionId" 
        <>  metavar "Transaction hash" 
        <> help "Hash for the transaction")

transactionCommandParser :: Parser EtherClientCommand 
transactionCommandParser = 
  TransactionQuery 
  <$> ipcPathParser <*> addressParser <*> transactionIdParser

blockBrowserCommandParser :: Parser EtherClientCommand 
blockBrowserCommandParser =  
  BlockBrowser
  <$> ipcPathParser <*> addressParser <*> blockIdParser <*> numberOfBlocksParser <*> defaultBlockChunksParser

commandParser :: Parser EtherClientCommand 
commandParser = blockBrowserCommandParser <|> transactionCommandParser

main :: IO () 
main = 
  etherClientCommandHandler =<< execParser opts 
    where 
      opts = info
              (commandParser <**> helper)   
              (fullDesc
                <> progDesc "Eth client over IPCPATH"
                <> header "EthClient - to communicate on ipc"
                )

etherClientCommandHandler :: EtherClientCommand -> IO ()
etherClientCommandHandler (TransactionQuery ipcPath address transactionId) = do
  setup DEBUG 
  addFileHandler "debug.log" DEBUG
  addFileHandler "info.log" INFO 
  queryTransactionIO ipcPath address transactionId >> return ()

etherClientCommandHandler (BlockBrowser ipcPath address blockId numberOfBlocks defaultBlocks) = do 
  setup DEBUG
  addFileHandler "debug.log" DEBUG
  addFileHandler "info.log" INFO 
  blockBrowserIO ipcPath address (blockId, numberOfBlocks, defaultBlocks)

transactionQuery = do 
  setup DEBUG
  [txId] <- getArgs
  addFileHandler "debug.log" DEBUG
  addFileHandler "info.log" INFO
  queryTransactionTestMethod txId >>= \x -> infoMessage $ Text.pack $ show x


blockBrowser = do 
  setup DEBUG
  [blockId, range] <- getArgs
  addFileHandler "debug.log" DEBUG
  addFileHandler "info.log" INFO
  testMethod (read blockId, read range)