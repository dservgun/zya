module Data.Zya.Utils.FileUtil
  (
    readInputLines
  )
where 

import Control.Exception
import System.IO(openFile, IOMode(..), hGetContents)


-- | Return a set of lines from a file using a bracket.
readInputLines :: FilePath -> IO [String] 
readInputLines aFile = do
  bracket (openFile aFile ReadMode) 
          (\_ -> return()) $ \h -> do 
            contents <- hGetContents h            
            return $ Prelude.lines contents