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
      _runHandler :: ReaderT CPSM (StateT [Command] IO) a
    } deriving(
        Applicative, 
        Functor, 
        Monad, MonadIO, 
        MonadReader CPSM, 
        MonadState [Command])



handleMessages :: Command -> LocalMessageHandler Command
handleMessages messageL = do 
    (command, pProcessId, sServer, mMessageDistributionStrategy) <- ask
    lastCommand <- get
    -- TODO : Complete the implementation.
    return messageL


