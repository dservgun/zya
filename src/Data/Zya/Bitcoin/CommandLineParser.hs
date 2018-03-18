module Data.Zya.Bitcoin.CommandLineParser
(
  mainCLI
) where
import System.Log.Logger
import Data.Zya.Utils.Logger
import Data.Text as Text
import System.Environment(getArgs)
import Options.Applicative
import Data.Semigroup((<>))
import Data.Zya.Bitcoin.Common as BCommon
import Data.Zya.Bitcoin.Block(BlockHeight(..))
import Data.Zya.Bitcoin.BitcoinSession

data ClientCommand = 
  CreateAddresses {_unAddressCount :: Int}
  | SearchTransactions 
      {_accounts :: FilePath
        , _blockHeight :: BlockHeight
        , _range :: Int
        , _addresses :: FilePath}
  deriving (Show)

parseAddressCount :: Parser ClientCommand
parseAddressCount = 
  CreateAddresses <$> option auto (long "Number of addresses" <> short 'n')

handleCommand :: ClientCommand -> IO ()
handleCommand (CreateAddresses n) = generateAddresses n 
handleCommand (SearchTransactions accountsFile b range c) = do 
    accFile <- return accountsFile
    listAddresses <- readInputLines c
    query' <- return $ Prelude.map (\x -> Address $ strip $ Text.pack x) listAddresses
    mapM (\ bl -> searchTransactions 
                      accFile
                      $ BlockQuery (bl, query')) $ Prelude.take range $ [b .. ]
    return ()
    

rangeParser :: Parser Int 
rangeParser = option auto (long "Range of blocks" <> short 'r' <> value 10)
accountsFileParser :: Parser FilePath 
accountsFileParser = option str (long "accounts file" <> short 'a')

blockHeightParser :: Parser BlockHeight 
blockHeightParser = 
  BlockHeight <$> (option auto (long "Block height" <> short 'b'))

addressesFileParser :: Parser FilePath 
addressesFileParser = 
  option str (long "addresses file" <> short 'd')

searchTransactionsParser :: Parser ClientCommand
searchTransactionsParser = 
    SearchTransactions <$> 
      accountsFileParser <*> blockHeightParser <*> rangeParser <*> addressesFileParser

commandParser = searchTransactionsParser <|> parseAddressCount 

mainCLI = do
  comm <- execParser opts 
  handleCommand comm
  where 
    opts = 
      info 
        (commandParser <**> helper) 
        (fullDesc 
            <> progDesc "Btc client"
            <> header "Btc client")
