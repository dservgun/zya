{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Data.Zya.Core.Internal.WebserviceProtocolHandler where



import Conduit
import Control.Concurrent.Async as Async (async, cancel, concurrently, waitAny)
import Control.Concurrent.STM.Lifted
import Control.Exception
import Control.Monad (void, guard)
import Control.Monad.Catch as Catch
import Control.Monad.Trans.Reader
import Data.Monoid ((<>))
import Data.Text as Text (Text, take, pack)
import Data.Typeable
import Data.UUID.V1(nextUUID)
import Data.Zya.Core.Service
import Network.WebSockets.Connection as WS (Connection, sendTextData, receiveData)


newtype ProtocolHandler a =
      ProtoHandler {
        _runConn :: ReaderT (WS.Connection, Server, MessageDistributionStrategy) IO a
      }
      deriving
      (
        Functor,
        Applicative,
        Monad,
        MonadIO)


newtype WebServiceErrorCall = WebServiceErrorCall Text deriving (Typeable)

-- Note: How long should error log lines be.
instance Show WebServiceErrorCall where
  show (WebServiceErrorCall aText) = "WebServiceErrorCall " <> (show $ Text.take 60 aText)


removeConn :: ClientIdentifier -> ProtocolHandler(WS.Connection, ClientIdentifier)
removeConn clientIdentifier = do
  (conn, app, _) <- ProtoHandler ask
  _ <- liftIO $ atomically $ deleteConnection app clientIdentifier
  return (conn, clientIdentifier)


addConn :: ProtocolHandler (WS.Connection, ClientIdentifier)
addConn = do
  (conn, app, _) <- ProtoHandler ask
  nextId <- (\x -> Text.pack $ show x) <$> liftIO nextUUID
  _ <- liftIO $ atomically $ addConnection app (ClientIdentifier nextId) conn
  return (conn, ClientIdentifier nextId)


handleConnectionException ::
  (MonadIO m, Show a) => Server -> ClientIdentifier -> a -> m Text
handleConnectionException app identifier a = do
    _ <- (liftIO $ atomically $ deleteConnection app identifier)
    return $ Text.pack (show a)
-- At this point we can safely be in the io monad, though adding a monad logger might
-- be beneficial.
-- readerThread :: ProtocolHandler ()
readerThread :: forall (m :: * -> *) b . MonadIO m =>
    (Connection, Server, ClientIdentifier) -> m b
readerThread (conn, app, identifier) = do
  liftIO $ do
    currentMessage <- atomically $ getNextLocalMessage app identifier
    WS.sendTextData conn (Text.pack $ show currentMessage)
      `Catch.catch`
        (\a@(SomeException _) -> void $ handleConnectionException app identifier a)
  readerThread (conn, app, identifier)

writerThread :: (Connection, Server, ClientIdentifier, Bool) -> IO (Text)
writerThread (conn, app, identifier, exit) = do
  (command :: Text) <-
    WS.receiveData conn  `Catch.catch`
          (\a@(SomeException _) -> do
              _ <- handleConnectionException app identifier a
              writerThread (conn, app, identifier, True))
  guard (exit == False)
  _ <- writerThread(conn, app, identifier, exit)
  return command


protocolHandler :: ProtocolHandler WS.Connection
protocolHandler = do
  (conn, app, strategy) <- ProtoHandler ask
  (_ , cid@(ClientIdentifier _)) <- addConn
  a <- liftIO . liftIO $ Async.async (readerThread (conn, app, cid))
  b <- liftIO . liftIO $ Async.async (writerThread (conn, app, cid, False))
  _ <- liftIO $ messagesTillNow app cid strategy
  _ <- liftIO $ Async.waitAny [a, b]
  _ <- removeConn cid

  return conn
