module Data.Zya.Kafka.Client where

import Network.Socket
import Network.Socket.ByteString
import Data.ByteString
import Data.Monoid
import Data.Set as Set
import Data.Zya.Kafka.Util
import Data.Word

blockSize :: Int 
blockSize = 4096

sendMessage :: ByteString -> FilePath -> IO ByteString
sendMessage aValue aFilePath = do 
  sock <- domainSocket aFilePath 
  _ <- Network.Socket.ByteString.sendAll sock aValue
  msg <- Network.Socket.ByteString.recv sock blockSize
  return msg

sendMessageOverSocket :: ByteString -> HostName -> ServiceName -> IO String
sendMessageOverSocket aValue hostName portNumber = do 
  sock <- socketBased hostName portNumber  
  _ <- Network.Socket.ByteString.sendAll sock aValue
  throttleTime <-Network.Socket.ByteString.recv sock 4
  let w = wordArrayToInt32 $ unpack throttleTime
  Prelude.putStrLn ("Read message. " <> show w) 
  return "punt"

domainSocket :: FilePath -> IO (Socket) 
domainSocket filePath = do 
  sock <- socket AF_UNIX Stream 0 
  connect sock (SockAddrUnix filePath)
  return sock
  
socketBased :: HostName -> ServiceName -> IO Socket
socketBased hostName portNumber = do 
    addrInfos <- getAddrInfo Nothing (Just hostName) (Just portNumber)
    -- Empty list will throw an exception, we dont need 
    -- to check for a safe head in this case.
    let serverAddr = Prelude.head addrInfos
    sock <- socket(addrFamily serverAddr) Stream defaultProtocol 
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serverAddr)
    return sock




data APIVersion = 
  APIVersion {versionString :: String, version :: Int}
  deriving(Show, Eq, Ord)


kafkaVersions :: [(String, Int)] -> [APIVersion] 
kafkaVersions = \x -> (uncurry APIVersion) <$> x



{--

    "0.8.0" -> KAFKA_0_8_0,
    "0.8.1" -> KAFKA_0_8_1,
    "0.8.2" -> KAFKA_0_8_2,
    "0.9.0" -> KAFKA_0_9_0,
    // 0.10.0-IV0 is introduced for KIP-31/32 which changes the message format.
    "0.10.0-IV0" -> KAFKA_0_10_0_IV0,
    // 0.10.0-IV1 is introduced for KIP-36(rack awareness) and KIP-43(SASL handshake).
    "0.10.0-IV1" -> KAFKA_0_10_0_IV1,
    "0.10.0" -> KAFKA_0_10_0_IV1,

    // introduced for JoinGroup protocol change in KIP-62
    "0.10.1-IV0" -> KAFKA_0_10_1_IV0,
    // 0.10.1-IV1 is introduced for KIP-74(fetch response size limit).
    "0.10.1-IV1" -> KAFKA_0_10_1_IV1,
    // introduced ListOffsetRequest v1 in KIP-79
    "0.10.1-IV2" -> KAFKA_0_10_1_IV2,
    "0.10.1" -> KAFKA_0_10_1_IV2,
    // introduced UpdateMetadataRequest v3 in KIP-103
    "0.10.2-IV0" -> KAFKA_0_10_2_IV0,
    "0.10.2" -> KAFKA_0_10_2_IV0,
    // KIP-98 (idempotent and transactional producer support)
    "0.11.0-IV0" -> KAFKA_0_11_0_IV0,
    // introduced DeleteRecordsRequest v0 and FetchRequest v4 in KIP-107
    "0.11.0-IV1" -> KAFKA_0_11_0_IV1,
    // Introduced leader epoch fetches to the replica fetcher via KIP-101
    "0.11.0-IV2" -> KAFKA_0_11_0_IV2,
    "0.11.0" -> KAFKA_0_11_0_IV2,
    // Introduced LeaderAndIsrRequest V1, UpdateMetadataRequest V4 and FetchRequest V6 via KIP-112
    "1.0-IV0" -> KAFKA_1_0_IV0,
    "1.0" -> KAFKA_1_0_IV0


--}
supportedVersions :: Set APIVersion
supportedVersions = 
    Set.fromList $ 
    kafkaVersions
      [
      ("0.8.0.X", 0)
      , ("0.8.1.x", 1)
      , ("0.8.2.x", 2) 
      , ("0.9.0.x", 3)
      , ("0.10.0-IV0", 4)
      , ("0.10.0-IV1", 5)
      , ("0.10.1-IV0", 6)
      , ("0.10.1-IV1", 7)
      , ("0.10.1-IV2", 8)
      , ("0.10.2-IV0", 9)
      , ("0.11.0-IV0", 10)
      , ("0.11.0-IV1", 11)
      , ("0.11.0-IV2", 12)
      , ("1.0-IV0", 13)
      ]



data APIKey = APIKey {messageType :: String, key :: Int} deriving (Show, Eq, Ord) 

apiKeys :: [(String, Int)] -> [APIKey]
apiKeys = \x -> uncurry APIKey <$> x

apiDictionary :: Set APIKey
apiDictionary = 
  Set.fromList $
    apiKeys
    [
      ("ProduceRequest", 0)
    , ("FetchRequest", 1)
    , ("OffsetRequest", 2)
    , ("MetadataRequest", 3)
    , ("OffsetCommitRequest", 8)
    , ("OffsetFetchRequest", 9)
    , ("GroupCoordinatorRequest", 10)
    , ("JoinGroupRequest", 11)
    , ("HeartbeatRequest", 12)
    , ("LeaveGroupRequest", 13)
    , ("SyncGroupRequest", 14)
    , ("DescribeGroupRequest", 15) 
    , ("ListGroupsRequest", 16)
  ]


apiKeyFor :: String -> APIKey 
apiKeyFor aString = 
  Prelude.head (Set.elems filteredSet)
  where 
    filteredSet = Set.filter (\(APIKey k _) -> aString == k) apiDictionary

data ErrorCodes = ErrorCode {code :: Int, meaning :: String} deriving (Show, Eq, Ord)

errorCodes :: [(Int, String)] -> Set ErrorCodes
errorCodes aList = 
  Set.fromList $ uncurry ErrorCode <$> aList

knownErrors :: Set ErrorCodes
knownErrors = 
  errorCodes 
    [
      (0, "NoError")
      ,(-1, "Unknwon") 
      , (1, "OffsetOutOfRange")
      , (2, "CorrputedMessage")
      , (3, "UnknownTopicOrPartition")
      , (4, "InvalidMessageSize")
      , (5, "LeaderNotAvailable")
      , (6, "NotLeaderForPartition")
      , (7, "RequestTimedOut")
      , (8, "BrokerNotAvailable")
      , (9, "ReplicaNotAvailable")
      , (10, "MessageSizeTooLarge")
      , (11, "StaleControllerEpoch")
      , (12, "OffsetMetadataTooLarge")
      , (14, "GroupLoadInProgress")
      , (15, "GroupCoordinatorNotAvailable")
      , (16, "NotCoordinatorForGroup")
      , (17, "InvalidTopic")
      , (18, "RecordListTooLarge")
      , (19, "NotEnoughReplicas")
      , (20, "NotEnoughReplicasAfterAppend")
      , (21, "InvalidRequiredAcks")
      , (22, "IllegalGeneration")
      , (23, "InconsistentGroupProtocol")
      , (24, "InvalidGroupId")
      , (25, "UnknownMemberId")
      , (26, "InvalidSessionTimeout")
      , (27, "RebalanceInProgress")
      , (28, "InvalidCommitOffsetSize")
      , (29, "TopicAuthorizationFailed")
      , (30, "GroupAuthorizationFailed")
      , (31, "ClusterAuthorizationFailed")
    ]



preferredVersion :: Int
preferredVersion = version $ uncurry APIVersion $ ("0.11.0-IV2", 12)

metadataRequestKey :: Int
metadataRequestKey = key $ apiKeyFor "MetadataRequest"

newtype CorrelationId = CorrelationId {_unCorr :: Int} deriving (Show, Ord, Eq)
newtype ClientId = ClientId {_unClientId :: Int} deriving(Show, Ord, Eq)
createMetadataRequest ::  CorrelationId -> ClientId -> ByteString
createMetadataRequest cor client
  = pack $ createMetadataRequestBA cor client

createMetadataRequestBA :: CorrelationId -> ClientId -> [Word8]
createMetadataRequestBA (CorrelationId corId) (ClientId clId) =
    int16ToWord8Array(metadataRequestKey)
    <> int16ToWord8Array(preferredVersion)
    <> int32ToWord8Array(corId)
    <> int32ToWord8Array(clId) 
    <> int32ToWord8Array(-1)

