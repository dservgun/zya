{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Zya.Bitcoin.BitcoinSession
(
  HostEndPoint(..)
  , GeneralSessionParameters(..)
  , addresses -- TODO remove this and read from a file.
  , searchTransactions
  , createAddresses
  , generateAddresses
) where

import Control.Exception.Safe
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Control.Monad.State as State
import Control.Monad.Trans(liftIO)
import Data.Aeson
import Data.Aeson.Lens(key)
import Data.Monoid
import Data.Text as Text
import Data.Text.IO as TextIO
import Data.Zya.Bitcoin.Block
import Data.Zya.Bitcoin.Common as BCommon
import Data.Zya.Bitcoin.JsonRPC hiding (getListReceivedByAddress)
import Data.Zya.Bitcoin.RawTransaction as RawTransaction
import Network.HTTP.Client hiding(responseBody)
import Network.Socket
import Network.Wreq as Wreq
import Network.Wreq.Types as WreqTypes
import qualified Network.Wreq.Session as WreqSession
import System.IO
import Data.Zya.Utils.Logger(debugMessage, infoMessage)
import Data.Zya.Bitcoin.Config

data ResultSetSize = ResultSet Int | AllRows deriving (Show)

instance Num ResultSetSize where 
  ResultSet a + ResultSet b = ResultSet (a + b)
  ResultSet _ + AllRows = AllRows
  AllRows + _ = AllRows

  ResultSet a * ResultSet b = ResultSet (a * b) 
  _ * AllRows = AllRows 
  AllRows * _ = AllRows 

  abs (ResultSet a) = ResultSet (abs a)
  abs AllRows = AllRows
  signum (ResultSet a) = ResultSet (signum a) 
  signum AllRows = AllRows 
  fromInteger = ResultSet . fromIntegral
  negate (ResultSet a) = ResultSet . negate $ a
  negate AllRows = AllRows

data GeneralSessionParameters = GeneralSessionParameters {
  transactionCount :: Integer
  , resultSize :: ResultSetSize
} deriving(Show)

data HostEndPoint = HostEndPoint {
  hostName :: HostName
  , serviceName :: ServiceName
  , userName :: UserName
  , password :: Password 
} deriving(Show)

createConfig :: String -> String -> String -> SessionConfig 
createConfig userName' password' port' = 
    let 
      sessParams = GeneralSessionParameters 1000 $ ResultSet 10
      hostEndPoint' = HostEndPoint "127.0.0.1" port' (UserName userName') (Password password')
    in 
      SessionConfig 1 hostEndPoint' sessParams "gl.csv" "accounts.txt"

endPoint :: HostEndPoint -> String 
endPoint (HostEndPoint hostName' serviceName' (UserName user') (Password pass')) = 
    "http://" <> user' <> ":" <> pass' <> "@" <> hostName' <> ":" <> serviceName'


data SessionConfig = SessionConfig {
  startRequestId :: Integer
  , hostEndPoint :: HostEndPoint
  , sessionParameters :: GeneralSessionParameters
  , outputFile :: FilePath
  , inputFile :: FilePath
} deriving(Show)

data SessionState = SessionState {
  nextRequestId :: RequestId
}

createDefaultState :: SessionState
createDefaultState = 
    SessionState 
      (RequestId 1) 


instance Show SessionState where
  show :: SessionState -> String 
  show (SessionState r) = show ("Session state : " :: String) <> (show r)

newtype Application a = Application {
  runA :: ReaderT SessionConfig (StateT SessionState IO) a
} deriving(Functor, Applicative, Monad, MonadIO, MonadReader SessionConfig, MonadState SessionState)

newtype SessionRequest = SessionRequest {_unReq :: Text} deriving (Show)
newtype SessionResponse = SessionResponse {_unRes :: Text} deriving(Show)

-- | Why do we need this?
fullyFormedEndPoint :: HostEndPoint -> String
fullyFormedEndPoint h  = endPoint h

-- convenience functions 
readHeaderInformation :: Application (String, Wreq.Options, ResultSetSize)
readHeaderInformation = do 
  (resultRows, hEndPoint) <- asks (\e -> (resultSize $ sessionParameters $ e, hostEndPoint e))
  let opts = defaults
  return(fullyFormedEndPoint hEndPoint, opts, resultRows)  

doPostWithS :: (Postable a, Show a) => Wreq.Options -> WreqSession.Session -> String -> a -> IO (Maybe Value)
doPostWithS opts session endPoint' aRequest = do 
  r <- asValue =<< WreqSession.postWith opts session endPoint' aRequest
  let respBody = r ^? responseBody . key "result"
  if respBody ==  Nothing then 
    return $ Just $ (String $ Text.pack $ "Failed to return response : " <> (show aRequest))
  else 
    return respBody

doPost :: (Postable a, Show a ) => Wreq.Options -> String -> a -> IO (Maybe Value)
doPost opts endPoint' aRequest = do
  r <- asValue =<< postWith opts endPoint' aRequest :: IO (Response Value)
  let respBody = r ^? responseBody . key "result"
  if respBody ==  Nothing then 
    return $ Just $ (String $ Text.pack $ "Failed to return response : " <> (show aRequest))
  else 
    return respBody

toTransaction :: Maybe Value -> Result RawTransaction
toTransaction Nothing = Error $ "Unable to parse" 
toTransaction (Just x) = fromJSON x  

transactionDetailsRequest :: SessionState -> Text -> Value
transactionDetailsRequest (SessionState nReqId) anId =
    getRawTransaction nReqId anId

transactionDetailsInBulk :: [Text] -> Application[Result RawTransaction]
transactionDetailsInBulk requestIds = do 
  (_, fEndPoint) <- asks (\e -> (e , endPoint . hostEndPoint $ e))
  currentState <- State.get  
  requestObjects <- 
        mapM (\requestId -> do 
                  (SessionState nReqId') <- State.get
                  let r = transactionDetailsRequest currentState requestId
                  State.modify(\s -> s {nextRequestId = nReqId' + 1})
                  return (r, requestId)
                  ) requestIds
  resp <- 
      liftIO $ WreqSession.withSession $ \session ->  
                mapM (\(requestObj, requestId) -> do
                            debugMessage $ Text.pack $ show requestObj
                            handle(\e@(SomeException _) -> 
                                          (debugMessage $ requestId <> ":" <> (Text.pack $ show e)) >> return Nothing) $  
                                doPostWithS 
                                defaults 
                                session 
                                fEndPoint 
                                requestObj) requestObjects

  return $ fmap toTransaction resp

addresses :: [AccountAddress]
addresses = AccountAddress 
            <$> 
            ["16R9ffu5BPcKHoy3dvZLQeghqHgHecEVWF", "1Pc2hje5qW62oajtZqayfs83bcSn2EiGbJ"
            , "15fnGtAJrqzct2fyE4SpGFQBKMnWWmv5fs"
            , "1DZzpDWEacmFVsw8KTRB6uqVxf7nAYdzmW"
            , "1C6nxuqAmytbXR162nC3Xw2TMsYkQJNw9C"]

writeToFileWM :: FilePath -> IOMode -> Text -> IO ()
writeToFileWM aFile aMode aMessage = 
  bracket (openFile aFile aMode) (hClose) $ \h -> do 
    TextIO.hPutStrLn h aMessage

writeToFile :: [Text] -> FilePath -> IOMode -> IO () 
writeToFile messages aFile aMode = 
  bracket (openFile aFile aMode) (hClose) $ \h -> do 
    TextIO.hPutStrLn h $ Text.unlines messages


readInputAccounts :: FilePath -> IO [AccountAddress]
readInputAccounts aFile = 
  readInputLines aFile >>= \line -> return $ fmap (AccountAddress . Text.pack) line

{-- | 
  Search for all transactions for a block query.
  * Retrieve the block hash for a given block, to return all the transactions for the block.
  * For each block, fetch the raw transaction. 
  * Filter all transactions that have the address in vout.  
 2019  ~/bitcoin/src/bitcoin-cli getblockhash 504347 true

 2021  ~/bitcoin/src/bitcoin-cli getblock 00000000000000000002d265bc588c25eb0619b1ba10282a2881779bdaf71a45
 2022  ~/bitcoin/src/bitcoin-cli getblock 00000000000000000002d265bc588c25eb0619b1ba10282a2881779bdaf71a45 | less
 2023  ~/bitcoin/src/bitcoin-cli help | less
 2024  ~/bitcoin/src/bitcoin-cli getrawtransaction "21cce6eb5d6dbc86e9ff89dc24217fc4d2de1eae0037517d81225646c2f67fec"
 2025  ~/bitcoin/src/bitcoin-cli getrawtransaction "21cce6eb5d6dbc86e9ff89dc24217fc4d2de1eae0037517d81225646c2f67fec" true
 2026  ~/bitcoin/src/bitcoin-cli getrawtransaction "21cce6eb5d6dbc86e9ff89dc24217fc4d2de1eae0037517d81225646c2f67fec" true | less 
 2027  ~/bitcoin/src/bitcoin-cli getblock 00000000000000000002d265bc588c25eb0619b1ba10282a2881779bdaf71a45 | less
 2028  ~/bitcoin/src/bitcoin-cli getrawtransaction "21cce6eb5d6dbc86e9ff89dc24217fc4d2de1eae0037517d81225646c2f67fec" true | less 

--}

maybeToResult :: Maybe (Result a) -> Result a 
maybeToResult Nothing = Error "Nothing to do"
maybeToResult (Just a) = a

isSuccessR :: Result RawTransaction -> Bool
isSuccessR (Success _) = True
isSuccessR _ = False

successR :: [Result RawTransaction] -> [RawTransaction] 
successR aList = 
    Prelude.map(\(Success y) -> y) $ Prelude.filter(isSuccessR) aList

queryMatches :: BlockQuery -> RawTransaction -> Bool
queryMatches (BlockQuery (_, addresses')) aRawTransaction = 
    Prelude.foldr 
      (\ele acc -> acc || ele) 
      False 
      $ Prelude.map(\x -> hasVOAddress x aRawTransaction) addresses'

queryMatchesR :: BlockQuery -> Result RawTransaction -> Bool 
queryMatchesR q (Success t) = queryMatches q t 
queryMatchesR _ _ = False

printDebugMessages :: (Show a) => [a] -> IO ()
printDebugMessages aList = mapM_ (\x -> debugMessage $ Text.pack $ show x) aList 

fetchBlockTransactionsWithSession :: BlockQuery -> Block -> Application [RawTransaction]
fetchBlockTransactionsWithSession query@(BlockQuery (_, addresses')) aBlock = do
  let transactions = transactionList aBlock 
  list <- transactionDetailsInBulk transactions
  list2 <- return $ Prelude.filter(queryMatchesR query) list
  listTrimmed <- 
    return $ fmap (\x -> fmap (\y -> filterVOAddresses addresses' y) x) list2
  listTrimmedWithAddresses <- 
    return $ Prelude.filter (\y -> conv $ fmap filterTransactionsWithVoutAddresses y) listTrimmed
  listTrimmedWithNoEmptyAddresses <-
    return $ Prelude.map (\y -> fmap filterVOWithAddressesRawTrans y) listTrimmedWithAddresses
  liftIO $ printDebugMessages $ successR listTrimmedWithNoEmptyAddresses
  return $ successR listTrimmedWithNoEmptyAddresses
  where 
    conv :: Result Bool -> Bool
    conv (Success True) = True 
    conv _  = False
  
createAddress :: Application(Result Address, Result Address)
createAddress = do 
  (fullyFormedEndPoint', opts, _) <- readHeaderInformation 
  (SessionState nReqId) <- State.get 
  let req = getCreateNewAddress nReqId 
  response <- liftIO $ doPost opts fullyFormedEndPoint' req
  let returnedAddress = maybeToResult $ fromJSON <$> response
  State.modify(\s -> s {nextRequestId = nReqId + 1})
  (SessionState nReqId') <- State.get
  responseAddress <-
      case returnedAddress of 
        Success address -> do 
          let addressDetails = getDumpPrivKey nReqId' address
          dumpPrivKey <- liftIO $ doPost opts fullyFormedEndPoint' addressDetails
          return $ maybeToResult $ fromJSON <$> dumpPrivKey
        Error _ -> return $ Error "Invalid address"
  return (returnedAddress, responseAddress) 

createAddressesM :: Int -> Application[(Result Address, Result Address)]
createAddressesM n = replicateM n createAddress 

fetchBlockDetails :: BlockQuery -> BlockHash -> Application [RawTransaction]
fetchBlockDetails blockQuery aHash = do 
  (fullyFormedEndPoint', opts, _) <- readHeaderInformation
  (SessionState nReqId) <- State.get
  let req = getBlock nReqId aHash 
  response <- liftIO $ doPost opts fullyFormedEndPoint' req  
  --putStrLnA $ Prelude.take 1024 $ show response
  State.modify(\s -> s {nextRequestId = nReqId + 1})
  block <- return $ maybeToResult $ fromJSON <$> response
  case block of 
    Success bl -> fetchBlockTransactionsWithSession blockQuery bl
    Error _ -> return []



fetchBlockHash :: BlockQuery -> Application [RawTransaction]
fetchBlockHash bQ@(BlockQuery blockQuery) = do 
  (fullyFormedEndPoint', opts, _) <- readHeaderInformation
  (SessionState nReqId) <- State.get
  let req = getBlockHash nReqId $ fst blockQuery
  response <- liftIO $ doPost opts fullyFormedEndPoint' req  
  -- putStrLnA $ show response
  State.modify(\s -> s {nextRequestId = nReqId + 1})
  resp <- return $ maybeToResult $ fromJSON <$> response
  case resp of 
    Success s -> fetchBlockDetails bQ s
    Error _ -> return []

searchTransactions :: FilePath -> BlockQuery -> IO ([RawTransaction], SessionState)
searchTransactions inputFileConfig blockQuery = do 
  config <- defaultFileLocation >>= readConfig 
  user <- btcUserName config
  passwordL <- btcPassword config
  port' <- btcRpcPort config
  infoMessage $ Text.pack $ "Query " <> show blockQuery
  let 
    config1 = createConfig (Text.unpack user) (Text.unpack passwordL) (Text.unpack port')
    initialState = createDefaultState
  _ <- readInputAccounts (inputFileConfig)
  result <- (runStateT (runReaderT (runA $ fetchBlockHash blockQuery) config1) initialState)
  infoMessage "--------------------------------"
  infoMessage $ Text.pack $ show result 

  csvResults <- 
    return $ 
        (Prelude.map(\s -> rawTransactionAsCSV (AccountAddress "t")(Address "a") s) $ fst result)
  infoMessage $ Text.pack $ show csvResults
  writeToFileWM (outputFile config1) AppendMode header' -- Write the header, append messages.
  writeToFile csvResults (outputFile config1) AppendMode
  return result
  where 
    header' = "Account, Address, Confirmation, Time, BlockTime, Amount, Address"



-- Create n number of addresses with public and private key
createAddresses :: Int -> IO ([(Result Address, Result Address)], SessionState)
createAddresses n =  
  defaultFileLocation >>= readConfig >>= \config1 -> do
  user <- btcUserName config1
  passwordL <- btcPassword config1
  port' <- btcRpcPort config1 
  let config = 
        createConfig (Text.unpack user) (Text.unpack passwordL) (Text.unpack port')
      initialState = createDefaultState
  runStateT (runReaderT (runA $ createAddressesM n) config) initialState


generateAddresses :: Int -> IO () 
generateAddresses n = 
  createAddresses n >>= \(add, _) -> 
  TextIO.putStrLn $ 
      intercalate "\n" $ 
        Prelude.map (\(Success (Address x), Success (Address y)) -> 
          (Text.pack $ show x) <> "->" <>  (Text.pack $ show y)) add
