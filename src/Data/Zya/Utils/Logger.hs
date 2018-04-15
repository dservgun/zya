module Data.Zya.Utils.Logger
  (
    setup
    , addFileHandler
    , addSysLogHandler
    , errorMessage
    , infoMessage
    , debugMessage
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
setup = \pri -> updateGlobalLoggerMIO rootLoggerName (setLevel pri)

addSysLogHandler :: Priority -> IO ()
addSysLogHandler pri = do
  l <- openlog "Data.Zya.Utils.Logger" [PID] USER pri
  updateGlobalLogger rootLoggerName (addHandler l)

addFileHandler :: FilePath -> Priority -> IO ()
addFileHandler fileName pri = do
  h <- fileHandler fileName pri >>= \lh -> return $
      setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg") 
  updateGlobalLogger rootLoggerName (addHandler h)



errorMessage :: (MonadIO m) => Text -> m ()
errorMessage message = liftIO $ errorM rootLoggerName (unpack message)

infoMessage :: (MonadIO m) => Text -> m () 
infoMessage message = liftIO $ infoM rootLoggerName (unpack message)

debugMessage :: (MonadIO m) => Text -> m ()
debugMessage message = liftIO $ debugM rootLoggerName (unpack message)

