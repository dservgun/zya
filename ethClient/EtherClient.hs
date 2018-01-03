module Main where
import Data.Zya.Ethereum.Sockets.Client
import System.Log.Logger
import Data.Zya.Utils.Logger
import Data.Text as Text
import System.Environment(getArgs)
import Options.Applicative
import Data.Semigroup((<>))
import Data.Zya.Ethereum.Internal.Types.RPCRequest
--main :: IO[([(SessionRequest, SessionResponse)], SessionState)]
-- TODO: Get parseopts.


data EtherClientCommand = 
  TransactionQuery {ipcPath :: FilePath, address :: String, transactionId :: Text}
    | BlockBrowser {ipcPath :: FilePath, address :: String, blockId :: Integer, numberOfBlocks :: Integer, defaultBlockChunks :: Integer}
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

addressParser :: String -> Parser String 
addressParser metaVarName= 
  strOption
    (
      long "address" 
      <> metavar metaVarName
      <> help ("The hash for the address")
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
  <$> ipcPathParser <*> (addressParser "Account address") <*> blockIdParser <*> numberOfBlocksParser <*> defaultBlockChunksParser

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

etherClientCommandHandler (SendTransaction commandType ipcPath accountAddress fromAddress toAddress gas gasPrice value txData nonce) = do 
  setup DEBUG 
  addFileHandler "debug.log" DEBUG 
  addFileHandler "info.log" INFO 
  sendTransactionMain ipcPath accountAddress ((Address fromAddress), Address toAddress, gas, gasPrice, value, txData, nonce)

transactionQuery = do 
  setup DEBUG
  [txId] <- getArgs
  addFileHandler "debug.log" DEBUG
  addFileHandler "info.log" INFO
  queryTransactionTestMethod txId >>= \x -> infoMessage $ Text.pack $ show x

