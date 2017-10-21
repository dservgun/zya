module Data.Zya.Utils.Logger where

import System.Log.Formatter
import System.Log.Handler(setFormatter)
import System.Log.Handler.Syslog
import System.Log.Logger

setup componentRoot =
  openlog "ZyaLogs" [PID] USER DEBUG
    >>= \s ->updateGlobalLogger componentRoot (addHandler s)

debugMessage = debugM

