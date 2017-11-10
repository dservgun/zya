module Data.Zya.Utils.Logger where

import System.Log.Handler.Syslog
import System.Log.Logger

setup :: String -> IO ()
setup componentRoot =
  openlog "ZyaLogs" [PID] USER DEBUG
    >>= \s ->updateGlobalLogger componentRoot (addHandler s)

debugMessage :: String -> String -> IO ()
debugMessage = debugM

