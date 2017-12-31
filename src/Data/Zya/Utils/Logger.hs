module Data.Zya.Utils.Logger
  (
    setup
    , addFileHandler
    , errorMessage
    , infoMessage
    , debugMessage
  )
 where

import Data.Text
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Handler.Syslog
import System.Log.Handler.Syslog
import System.Log.Logger



setup :: Priority-> IO ()
setup priority = do 
  updateGlobalLogger rootLoggerName (setLevel priority)
  return ()


addSysLogHandler priority = do
  l <- openlog "Data.Zya.Utils.Logger" [PID] USER priority
  updateGlobalLogger rootLoggerName (addHandler l)

addFileHandler fileName priority = do
  h <- fileHandler fileName priority >>= \lh -> return $
      setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg") 
  updateGlobalLogger rootLoggerName (addHandler h)

errorMessage :: Text -> IO ()
errorMessage message = errorM rootLoggerName (unpack message)

infoMessage :: Text -> IO () 
infoMessage message = infoM rootLoggerName (unpack message)

debugMessage :: Text -> IO ()
debugMessage message = debugM rootLoggerName (unpack message)

