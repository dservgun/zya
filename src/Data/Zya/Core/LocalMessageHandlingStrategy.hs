{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


-- Each service manages its own service access strategy. This 
-- might still result in a global storm on any one instance.
module Data.Zya.Core.LocalMessageHandlingStrategy (sendMessage, runMessageWriter) where 
import GHC.Generics (Generic)
import System.Environment(getArgs)

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Applicative((<$>), liftA2)
import Control.Exception

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State as State
import Control.Monad.Writer

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)

import Control.Monad.State 
import Control.Monad.Reader 
import Control.Monad.Writer

import Data.Zya.Core.Service
import Text.Printf
import Data.Zya.Core.ServiceTypes
import Data.Maybe

newtype AvailableServerState a = 
  ServerState {
    runServerState :: StateT(Maybe ProcessId) (ReaderT (ServiceProfile, FairnessStrategy) Process) a
    } deriving (
        Functor
        , Applicative
        , Monad
        , MonadIO 
        , MonadReader (ServiceProfile, FairnessStrategy) 
        , MonadState (Maybe ProcessId)
      )


sendMessage :: PMessage -> Server -> AvailableServerState ()
sendMessage aMessage server = do
  (serviceProfile, strategy) <- ask
  prevWriter <-  State.get 
  current <- liftIO $ atomically $ findAvailableService server serviceProfile strategy
  State.put(current)
  let sticky = sameAsBefore prevWriter current
  when sticky $  
        ServerState $ 
          lift $ lift $ say $ printf ("Sticky process.." <> show prevWriter <> " : " <> show current)
  return ()


-- If the process ids are the same, for successive messages, 
-- the process is stuck.
stickyProcess :: Maybe(ProcessId) -> Maybe(ProcessId) -> Bool
stickyProcess a b = fromMaybe False $ liftA2 (==) a b 

-- We first like to write and then read (or should we read first?)
runMessageWriter :: PMessage -> Server -> Process () 
runMessageWriter aMessage server = do 
  initWriter <- liftIO $ atomically $ findAvailableWriter server
  runReaderT (runStateT (runServerState $ sendMessage aMessage server) initWriter) (Writer, RoundRobin)
  return ()
