module Data.Zya.Utils.Logger where

import System.Log.Logger
import System.Log.Handler.Syslog 
import System.Log.Handler(setFormatter)
import System.Log.Formatter 

setup componentRoot = 
  openlog "ZyaLogs" [PID] USER DEBUG 
    >>= \s ->updateGlobalLogger componentRoot (addHandler s)

debugMessage = debugM 
