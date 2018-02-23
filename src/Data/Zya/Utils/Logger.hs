module Data.Zya.Utils.Logger
  (
    setup
    , addFileHandler
    , errorMessage
    , infoMessage
    , debugMessage
    , addFileHandler
  )
 where

import Data.Text
import Control.Monad.Trans
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Handler.Syslog
import System.Log.Logger



updateGlobalLoggerMIO :: (MonadIO m) => String -> (Logger -> Logger) -> m ()
updateGlobalLoggerMIO loggerName logTrans = liftIO $ updateGlobalLogger loggerName logTrans

setup :: (MonadIO m) => Priority-> m ()
setup priority = do 
  updateGlobalLoggerMIO rootLoggerName (setLevel priority)


addSysLogHandler priority = do
  l <- openlog "Data.Zya.Utils.Logger" [PID] USER priority
  updateGlobalLogger rootLoggerName (addHandler l)

addFileHandler fileName priority = do
  h <- fileHandler fileName priority >>= \lh -> return $
      setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg") 
  updateGlobalLogger rootLoggerName (addHandler h)



errorMessage :: (MonadIO m) => Text -> m ()
errorMessage message = liftIO $ errorM rootLoggerName (unpack message)

infoMessage :: (MonadIO m) => Text -> m () 
infoMessage message = liftIO $ infoM rootLoggerName (unpack message)

debugMessage :: (MonadIO m) => Text -> m ()
debugMessage message = liftIO $ debugM rootLoggerName (unpack message)

