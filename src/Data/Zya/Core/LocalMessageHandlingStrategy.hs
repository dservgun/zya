{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


-- Each service manages its own service access strategy. This
-- might still result in a global storm on any one instance.
module Data.Zya.Core.LocalMessageHandlingStrategy 
  (sendMessage
    , runMessageWriter
    , runComputeNodeEvent) where


import Control.Applicative((<$>), liftA2)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node as Node
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State as State
import Control.Monad.Trans
import Control.Monad.Writer
import Data.Aeson 
import Data.Binary
import Data.Maybe
import Data.Text(pack, unpack)
import Data.Text.Encoding(encodeUtf8)
import Data.Time(UTCTime, getCurrentTime)
import Data.Zya.Core.Internal.LocalMessage
import Data.Zya.Core.Service
import Data.Zya.Core.ServiceTypes
import Data.Zya.Utils.Logger
import GHC.Generics (Generic)
import System.Environment(getArgs)
import Text.Printf

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


liftPrintln :: String -> AvailableServerState ()
liftPrintln aString =
  ServerState $
    lift . lift . say $ printf $ aString <> "\n"

liftLiftIO :: IO a -> AvailableServerState a
liftLiftIO f = ServerState $ lift . liftIO $ f


sendMessage :: PMessage -> Server -> AvailableServerState ()
sendMessage aMessage server = do
  (serviceProfile, strategy) <- ask
  prevWriter <-  State.get
  current <- liftIO $ atomically $ findAvailableService server serviceProfile strategy
  currentTime <- liftIO getCurrentTime
  let sticky = stickyProcess prevWriter current
  when sticky $ liftLiftIO $ infoMessage $ pack ("Sticky process.." <> show prevWriter <> " : " <> show current)
  case current of
    Just x -> 
      liftLiftIO $ atomically $ sendRemote server x (aMessage, currentTime)
    Nothing -> 
      liftLiftIO $ debugMessage $ 
        pack ("No writer found " <> 
                show prevWriter <> " : " <> show current <> " : " <> show aMessage)

  State.put current
  return ()


-- If the process ids are the same, for successive messages,
-- the process is stuck.
stickyProcess :: Maybe ProcessId -> Maybe ProcessId -> Bool
stickyProcess a b = fromMaybe False $ liftA2 (==) a b


--runReaderT (runStateT (runServerState $ sendMessage aMessage server) initWriter) (Writer, RoundRobin)
runMessage :: PMessage -> Server -> ServiceProfile -> Process () 
runMessage aMessage aServer aServiceProfile = do
  publisher <- liftIO $ atomically $ findAvailableService aServer aServiceProfile RoundRobin 
  void $
    runReaderT 
      (runStateT 
      (runServerState $ sendMessage aMessage aServer) publisher) 
      (aServiceProfile, RoundRobin)

runComputeNodeEvent :: PMessage -> Server -> Process () 
runComputeNodeEvent aMessage server = runMessage aMessage server ComputeNode

-- We first like to write and then read (or should we read first?)
runMessageWriter :: PMessage -> Server -> Process ()
runMessageWriter aMessage server = runMessage aMessage server Writer


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
    lastCommand <- eitherDecodeStrict . encodeUtf8 <$> State.get :: LocalMessageHandler (Either String LocalMessage)
    commandHandler <- 
      case lastCommand of 
        Right localMessage -> happyPath localMessage >> return lastCommand
        Left errorMessage -> return lastCommand
    -- TODO : Complete the implementation.
    return messageL


