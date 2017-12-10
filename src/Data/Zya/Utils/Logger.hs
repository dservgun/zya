module Data.Zya.Utils.Logger where


import System.Log.Logger
import System.Log.Handler.Syslog
import Data.Text



setup :: Priority-> IO ()
setup priority = do 
  l <- openlog "Data.Zya.Utils.Logger" [PID] USER priority
  updateGlobalLogger rootLoggerName (addHandler l)
  updateGlobalLogger rootLoggerName (setLevel priority)
  return ()




errorMessage :: Text -> IO ()
errorMessage message = errorM rootLoggerName (unpack message)

infoMessage :: Text -> IO () 
infoMessage message = infoM rootLoggerName (unpack message)

debugMessage :: Text -> IO ()
debugMessage message = debugM rootLoggerName (unpack message)

