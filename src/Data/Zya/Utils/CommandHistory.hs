{-# LANGUAGE OverloadedStrings #-}
module Data.Zya.Utils.CommandHistory
(
  -- | Saves command history to a well known location.
  save
  , saveToLocation
) where 

import System.IO(openFile, hClose, hPutStrLn, IOMode(..))
import System.Directory
import System.Environment (getArgs) 
import Data.Monoid ((<>))
import Data.Text as Text
import Control.Exception(bracket)
-- | Save the command to a commandhistory file.

saveToLocation :: FilePath -> FilePath -> IO () 
saveToLocation directory file = do 
  putStrLn "Saving history.."
  createDirectoryIfMissing True directory
  args <- getArgs 
  let argString = Text.intercalate " " $ Text.pack <$> args 
  bracket 
    (openFile (directory <> "/" <> file) AppendMode)
    (hClose) $ \fileHandle -> hPutStrLn fileHandle $ show  argString

save = saveToLocation ".zya" "history.txt"