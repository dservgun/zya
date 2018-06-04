{-# LANGUAGE DeriveDataTypeable #-}
module Data.Zya.Ceph.LocalInstall where 

import System.Directory
import Data.Monoid((<>))
import System.IO
import Control.Exception.Safe
import Control.Monad

newtype CommandNotFoundException = 
    CommandNotFoundException {_commands :: [FilePath]}
      deriving(Show, Typeable)

instance Exception CommandNotFoundException
{-| 
  A simple installation for ceph-deploy. 
-}

newtype CephConf = CephConf {_file :: FilePath}

defaultCephConf :: FilePath -> CephConf 
defaultCephConf aDirectory = CephConf $ aDirectory <> "/" <> "ceph.conf"

{-| 
- Ensure that the following binaries are in the path.
- Ensure ssh to localhost without entering a password.
- 'dnsmasq'
- 'ceph-deploy'
-}


configure :: [FilePath] -> IO [Bool]
configure = 
  \commands -> 
      Prelude.mapM doesPathExist commands

{-
mkdir mycephfiles
cd mycephfiles
ceph-deploy new ceph-test-1
echo “osd crush chooseleaf type = 0” >> ceph.conf
echo “osd pool default size = 1” >> ceph.conf
-}
setupSingleNodeOsd :: Handle -> IO ()
setupSingleNodeOsd aHandle = do 
  hPutStrLn aHandle "osd crush chooseleaf type = 0"
  hPutStrLn aHandle "osd pool default size = 1"


createInstallation :: FilePath -> IO CephConf 
createInstallation aDirectory = do 
  let ceph'@(CephConf aFile) = defaultCephConf aDirectory
  handle <- openFile aFile WriteMode
  bracket (openFile aFile WriteMode) (hClose) (\handle -> setupSingleNodeOsd handle)
  return ceph'

checkInstallation = do 
  let commands = ["dnsmasq", "ceph-deploy"]
  notFounds <- 
      configure commands >>= \results ->  
        return $ 
          Prelude.filter (\x -> x == False) results
  if (Prelude.length notFounds > 0) then 
    throwIO $ CommandNotFoundException $ commands
  else 
    return ()