{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Zya.Core.Writer(
  -- * Writers that handle log events
  writer
  , handleRemoteMessage
  ) where

import GHC.Generics (Generic)
import System.Environment(getArgs)

import Control.Concurrent.STM
import Control.Applicative((<$>))
import Control.Exception

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Reader

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)

import Data.Binary
import Data.Data
import Data.Monoid((<>))
import Data.Text(pack, unpack, Text)
import Data.Time(UTCTime, getCurrentTime)
import Data.Typeable
import Data.Zya.Core.Service
import Text.Printf
import Data.Zya.Core.ServiceTypes
import Data.Zya.Persistence.Persistence(DBType, persist)


-- File path to actually do the logging.
-- Use conduits.
rootLocation :: FilePath 
rootLocation = "./tmp" 


type RemoteT = ReaderT (Server, PMessage) Process ()

{-| Transform the database error into a writer error. -}
mapError :: Either Text PMessage -> CreateStatus
mapError (Right e) = CreateStatus success
mapError (Left e) = CreateStatus e


persistMessage :: MessageT
persistMessage = do 
  -- Insert the topic into persistent store.
  -- return the status. Send the status to 
  -- some peers (need to decide that, could be all).
  p <- persist
  return $ CreateStatus "success??"


inform :: CreateStatus -> Process () 
inform = undefined




handleRemoteMessage server dbType connectionString aMessage@(CreateTopic aTopic) = do
  say $ printf ("Received message " <> (show aMessage))
  return ()

handleRemoteMessage server dbType connectionString aMessage@(ServiceAvailable serviceProfile pid) = do
  say $ printf ("Received message " <> (show aMessage))  
  _ <- liftIO $ atomically $ do 
      myPid <- getMyPid server
      addService server serviceProfile pid
      sendRemote server pid (GreetingsFrom Writer myPid)
  return ()

handleRemoteMessage server dbType connectionString aMessage@(GreetingsFrom serviceProfile pid) = do
  say $ printf ("Received message " <> (show aMessage))
  _ <- liftIO $ atomically $ do 
    addService server serviceProfile pid
  return ()

handleRemoteMessage server dbType connectionString aMessage@(WriteMessage publisher (messageId, topic, message)) = do
  say $ printf ("Received message " <> (show aMessage))
  status <- liftIO $ runReaderT persistMessage (dbType, connectionString, aMessage)
  say $ printf "Message persisted successfully %s " (show status)
  return ()

handleRemoteMessage server dbType connectionString unhandledMessage = 
  say $ printf ("Received unhandled message  " <> (show unhandledMessage))


handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification server notificationMessage = 
  say $ printf ("Monitor notification " <> (show notificationMessage))




eventLoop :: ServerReaderT ()
eventLoop = do
  serverConfiguration <- ask
  lift $ do 
    let sName = unpack $ serverConfiguration^.serviceName
    let serverL = serverConfiguration^.server 
    let profileL = serverConfiguration^.serviceProfile
    let dbTypeL = serverConfiguration^.dbType 
    let connectionDetailsL = serverConfiguration^.connDetails
    spawnLocal (proxyProcess serverL)
    forever $
      receiveWait
        [ 
        match $ handleRemoteMessage serverL dbTypeL connectionDetailsL
        , match $ handleMonitorNotification serverL
        , matchIf (\(WhereIsReply l _) -> l == sName) $
                handleWhereIsReply serverL Writer
        , matchAny $ \_ -> return ()      -- discard unknown messages
        ]

writer :: ServerReaderT () 
writer = do
  initializeProcess
  eventLoop
  return()


-- Some synonyms
success :: Text 
success = "Success"
