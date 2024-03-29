{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Ethereum.Utils.CommandLineParser 
(
  mainCLI  
)
where
import Data.Zya.Ethereum.Sockets.Client
import System.Log.Logger
import Data.Zya.Utils.Logger
import Data.Zya.Utils.CommandHistory as CommandHistory
import Data.Text as Text
import System.Environment(getArgs)
import Options.Applicative
import Data.Semigroup((<>))
import Control.Exception

import Data.Zya.Utils.JsonRPC(Address(..))

{-- | 
  * BlockBrowser notes
  -- addressFile : the input file.
  -- reconFile : an input file with potential set of transactions.  
--}
data EtherClientCommand = 
    BlockBrowser {ipcPath :: FilePath
                      , outputPath :: FilePath                  
                      , addressFile :: FilePath
                      , reconFile :: FilePath
                      , blockId :: Integer
                      , numberOfBlocks :: Integer
                      , parallelism :: Int -- We should use idris here.
                    }
    | SendTransaction {commandType :: String
                        , ipcPath :: FilePath
                        , address :: String
                        , fromAddress :: String
                        , toAddress :: String
                        , gas :: Integer
                        , gasPrice :: Integer
                        , value :: Integer 
                        , txData :: Integer 
                        , nonce :: Integer
                      }
    | TransactionQuery {ipcPath :: FilePath, address :: String, transactionId :: Text}

  deriving(Show)  



blockIdParser :: Parser Integer 
blockIdParser = 
  read <$> 
    strOption
      (long "blockId"
        <> metavar "BLOCK ID"
        <> help "Block id to start scanning from")

parallelismParser :: Parser Int 
parallelismParser  =
  read <$> 
    strOption 
      (long "parallelism" 
        <> metavar "Parallelism"
        <> help "Use a number that is a function of the cores.")
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

outputFileParser :: Parser FilePath 
outputFileParser = 
  strOption 
    (long "outputFile" <> metavar "OutputFile" <> help "The output file")
ipcPathParser :: Parser FilePath
ipcPathParser = 
  strOption
    (long "ipcPath"
     <> metavar "IPCPATH"
     <> help "The ipc path for the eth node") 

addressParser :: String -> Parser String 
addressParser metaVarName= 
  strOption
    (
      long metaVarName 
      <> metavar metaVarName
      <> help ("File containing hash for an address")
    )

transactionIdParser :: Parser Text 
transactionIdParser = 
  Text.pack
  <$> strOption 
      (long "transactionId" 
        <>  metavar "Transaction hash" 
        <> help "Hash for the transaction")

gasParser :: Parser Integer 
gasParser = 
  read <$> strOption 
            (long "gas" <> metavar "Amount of gas used" <> help "Amount of gas for a transaction")

gasPriceParser :: Parser Integer 
gasPriceParser = 
  read <$> strOption
            (long "gasPrice" <> metavar "Gas price" <> help "Gas price for the transaction")

valueParser :: Parser Integer 
valueParser = read<$> strOption (long "value" <> metavar "Transaction value" <> help "Transaction value in wei")

commandTypeParser :: Parser String
commandTypeParser = strOption (long "commandType" <> metavar "commandType" <> help "Command type")

-- TODO: Fix these.

txDataParser :: Parser Integer 
txDataParser = pure 0xffffff

nonceDataParser :: Parser Integer 
nonceDataParser = pure 0xfffff

transactionCommandParser :: Parser EtherClientCommand 
transactionCommandParser = 
  TransactionQuery 
  <$> ipcPathParser <*> (addressParser "Account address") <*> transactionIdParser

blockBrowserCommandParser :: Parser EtherClientCommand 
blockBrowserCommandParser =  
  BlockBrowser
  <$> ipcPathParser <*> outputFileParser 
    <*> (addressParser "addressFile")
    <*> (addressParser "reconFile")
    <*> blockIdParser <*> numberOfBlocksParser
    <*> parallelismParser


sendTransactionParser = 
  SendTransaction 
    <$> commandTypeParser 
    <*> ipcPathParser 
    <*> addressParser "AccountAddress"
    <*> addressParser "FromAddress"
    <*> addressParser "ToAddress"
    <*> gasParser 
    <*> gasPriceParser
    <*> valueParser
    <*> txDataParser
    <*> nonceDataParser
commandParser :: Parser EtherClientCommand 
commandParser = blockBrowserCommandParser <|> transactionCommandParser <|> sendTransactionParser 



defaultChunkSize :: Integer
defaultChunkSize = 1
etherClientCommandHandler :: EtherClientCommand -> IO ()
etherClientCommandHandler aCommand = do
  setup INFO 
  addFileHandler "eth.debug.log" DEBUG
  addFileHandler "eth.info.log" INFO 
  case aCommand of 
    TransactionQuery ipcPath address transactionId -> do
      queryTransactionIO ipcPath address [transactionId] >> return ()
    BlockBrowser ipcPath outputFile addressFile reconFile blockId numberOfBlocks parallelism -> 
      do 
        t <- browseBlocksAsync ipcPath outputFile addressFile 
                reconFile (blockId, numberOfBlocks, defaultChunkSize) parallelism -- Defaulting this to 1. Too many parameters.
        return ()
    SendTransaction 
        commandType ipcPath accountAddress fromAddress toAddress gas gasPrice value1 txData1 nonce1 ->
      sendTransactionMain ipcPath accountAddress ((Address fromAddress), Address toAddress, gas, gasPrice, value1, txData1, nonce1)    

mainCLI' :: IO ()
mainCLI' = do 
  CommandHistory.save
  etherClientCommandHandler =<< execParser opts 
    where 
      opts = info
              (commandParser <**> helper)   
              (fullDesc
                <> progDesc "Eth client over IPCPATH"
                <> header "EthClient - to communicate on ipc"
                )

mainCLI :: IO ()
mainCLI = do 
  putStrLn "Starting...."
  mainCLI' `catch` (\e@(SomeException c) -> errorMessage $ Text.pack $ show e)