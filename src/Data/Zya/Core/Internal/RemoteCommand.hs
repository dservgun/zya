{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Core.Internal.RemoteCommand where 

import Data.Binary
import Data.Text
import Data.Typeable
import GHC.Generics (Generic)
import System.Process (CreateProcess
    , proc
    , CreateProcess
    , StdStream(..)
    , createProcess
    , callProcess
    , std_out
    , cwd)


newtype Name = Name {_unName :: Text} deriving (Typeable, Generic, Show) 
newtype Version = Version {_unVersion :: Text} deriving (Typeable, Generic, Show) 
data Distribution = Debian | Ubuntu | NotSupported deriving (Typeable, Generic, Show)
newtype Library = 
  Library {_libTuple :: (Name, Version, Distribution)}
    deriving (Typeable, Generic, Show)

-- | Execute the command. 
-- | Going to assume that the command has all the depedencies
-- | needed to run, in case of shared object dependencies 
-- | that do not get linked at build time. Ideally running 
-- | a stand alone executable is preferable.

data RemoteCommand = 
    RemoteCommand {
    commandLine :: Text 
    , environment :: [(Text, Text)]
    , commandId :: Text 
    , libraries :: [Library]
  } deriving (Typeable,Generic, Show)

defaultCommand :: Text -> RemoteCommand 
defaultCommand anId = RemoteCommand {
  commandLine = "date"
  , environment = [] 
  , commandId = anId
  , libraries = []
}

execute :: RemoteCommand -> IO () 
execute r@(RemoteCommand commandLine env commandId libraries) = do
  _ <- createProcess (proc (Data.Text.unpack commandLine) [])
  return ()
  
instance Binary RemoteCommand
instance Binary Library
instance Binary Distribution 
instance Binary Version 
instance Binary Name