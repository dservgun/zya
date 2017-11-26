{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Zya.Core.Internal.LocalMessage
  (
    createMessageSummary
    , createMessageSummaryP
    , MessageId
    , Device
    , UserName
    , DeviceTimeStamp
    , LocalMessage(..)
    , commitMessage
  )

where

{-- |
  External messages that are supported by the cloud. Within the cloud messages use
  PMessage. Use LocalMessage when integrating an external client (not a member of the cloud)
--}

import Data.Time(UTCTime)
import Data.Text
import Data.Zya.Core.Internal.MessageDistribution
import Data.Zya.Core.Internal.ServerTypes
import Control.Distributed.Process
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson 
import Data.Text.Encoding

-- TODO: MessageId needs to be a newtype.
-- | Constructors.
createMessageSummaryP :: Text -> ProcessId -> LocalMessage
createMessageSummaryP messageIdL processIdL = createMessageSummary messageIdL $ pack . show $ processIdL

createMessageSummary :: Text -> Text -> LocalMessage
createMessageSummary messageIdL processIdL = MessageSummary "MessageSummary" messageIdL processIdL

{-- |
  Commit a message: update the read status of the message in the store. Replicate
  the status all members in the cloud.
--}
commitMessage :: UserName -> Device -> Topic -> MessageId -> UTCTime -> LocalMessage
commitMessage aUserName aDevice aTopic aMessageId timeStamp =
    Commit "Commit" aUserName aDevice aTopic aMessageId timeStamp

{-- |
  Login the user into the cloud. Check if the user exists in the cloud then ensure
    that the device is not already logged in. If the (user, device) pair is already
    logged in, send an error back and update a global greylist.
    If the device doesnt exist, then add the device and let the cloud know.
    Update any grey lists for the username device pair across the cloud.
--}
login :: UserName -> Device -> LocalMessage
login aUser aDevice = undefined



{-newtype ProtocolHandler a =
      ProtoHandler {
        _runConn :: ReaderT (WS.Connection, Server, MessageDistributionStrategy) IO a
      }
      deriving
      (
        Functor,
        Applicative,
        Monad,
        MonadIO)
-}


type CPSM = (Command, ProcessId, Server, MessageDistributionStrategy)
newtype LocalMessageHandler a =
    LMessageHandler {
      _runHandler :: ReaderT CPSM (StateT Command IO) a
    } deriving(
        Applicative, 
        Functor, 
        Monad, MonadIO, 
        MonadReader CPSM, 
        MonadState Command)

happyPath :: LocalMessage -> LocalMessageHandler LocalMessage 
happyPath aLocalMessage = do 
  case aLocalMessage of 
      Login _ _ _ _ -> handleLogin aLocalMessage 
      Logout _ _ _ _ -> handleLogout aLocalMessage 
      Session _ _ _ _ _ -> handleSession aLocalMessage 
      Topics _ _ _ _ _ -> handleTopics aLocalMessage 
      Publish _ _ _ _ _ _ _  -> handlePublish aLocalMessage 
      Commit _ _ _ _ _ _ -> handleCommit aLocalMessage 
      MessageSummary _ _ _ -> handleMessageSummary aLocalMessage



handleLogin :: LocalMessage -> LocalMessageHandler LocalMessage
handleLogin aLocalMessage = do 
  (command, pProcessId, sServer, mMessageDistributionStrategy) <- ask
  return aLocalMessage
  
handleLogout aLocalMessage = undefined 
handleSession aLocalMessage = undefined 
handleTopics aLocalMessage = undefined 
handlePublish aLocalMessage = undefined
handleCommit aLocalMessage = undefined 
handleMessageSummary aLocalMessage = undefined    


handleMessages :: Command -> LocalMessageHandler Command
handleMessages messageL = do 
    (command, pProcessId, sServer, mMessageDistributionStrategy) <- ask
    lastCommand <- eitherDecodeStrict . encodeUtf8 <$> get :: LocalMessageHandler (Either String LocalMessage)
    commandHandler <- 
      case lastCommand of 
        Right localMessage -> happyPath localMessage >> return lastCommand
        Left errorMessage -> return lastCommand
    -- TODO : Complete the implementation.
    return messageL


