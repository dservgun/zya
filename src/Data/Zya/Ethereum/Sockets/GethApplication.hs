{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Zya.Ethereum.Sockets.GethApplication
  (
    -- * Application
    EthereumSessionApp
    -- * runner
    , runA
    -- * Configuration and state.
    , SessionConfig(..)
    , SessionState(..) 
    , SessionStatus
    , SessionRequest
    , SessionResponse
  )
 where 

import Control.Applicative
import Control.Concurrent(forkIO)
import Control.Exception(bracket, handle, SomeException(..), catch)
import Control.Monad(forever, unless)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans(liftIO)
import Data.Aeson
import Network.Socket(Socket, close)
import Network.Socket.ByteString
import qualified Data.Text as T
import qualified Data.Text.IO as T 
import System.IO
import Text.Printf

--- Design
--- The application session contains a read only socket
--- the last request is the previous state

data SessionConfig = SessionConfig {
  startRequestId :: Integer
  , sessionSocket :: Socket
  , socketPath :: FilePath  
  , outputFileHandle :: Handle
  , accountAddress :: Integer
  , startBlock :: Integer
  , endBlock :: Integer
} deriving(Show)

data SessionState = SessionState {
  nextRequestId :: Int
  , curBlock :: Maybe Integer
} deriving(Show)

newtype EthereumSessionApp a = EthereumSessionApp {
  runA :: ReaderT SessionConfig (StateT SessionState IO) a
} deriving(Functor, Applicative, Monad, MonadIO, MonadReader SessionConfig, MonadState SessionState)

data SessionStatus = EthSuccess | EthFailure deriving (Show)

newtype SessionRequest = SessionRequest {_unReq :: T.Text} deriving (Show)
newtype SessionResponse = SessionResponse {_unRes :: T.Text} deriving(Show)




