module Data.Zya.Bitcoin.Config
  (
    readConfig
    , ConfigurationException
  ) where

import System.IO
import Data.Text as Text
import Data.Map as Map
import Data.Traversable
import Control.Exception.Safe
import Control.Applicative
import Control.Monad.IO.Class

-- A simple config file to read from a bitcoin.conf file.

type Config = Map Text Text

data ConfigurationException = 
    ConfigurationException FilePath [Text] deriving(Typeable, Show)

instance Exception ConfigurationException
isEqualChar :: Char -> Bool
isEqualChar aChar = aChar == '='


safeIndex :: Int -> [a] -> Maybe a 
safeIndex anIndex a = 
  if (Prelude.length a <= anIndex) then
    Nothing
  else 
    Just $ a !! anIndex

splitInWords :: Text -> Maybe (Text, Text) 
splitInWords aText = (,) <$> firstWord <*> secondWord
  where
    elements = Text.split isEqualChar aText
    firstWord = safeIndex 0 elements
    secondWord = safeIndex 1 elements

readConfig :: FilePath -> IO Config
readConfig aFile = do 
  bracket(openFile aFile ReadMode) (hClose) $ \fileHandle -> do
    content <- Text.pack <$> (hGetContents fileHandle)
    let lineArray = (Text.lines content)
    let larray = splitInWords <$> lineArray
    let result = fmap (Map.fromList) $ sequenceA larray
    putStrLn $ show larray
    case result of
      Just r -> return r 
      Nothing -> liftIO $ throwM $ ConfigurationException aFile $ lineArray