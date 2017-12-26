module Data.Zya.Ethereum.CLI where 

import Data.Text
import Data.Monoid
import Data.Set
import System.Environment(getArgs)
import Data.Zya.Ethereum.Internal.Types.RPCRequest(Address)



-- Informal requirements
-- It should handle command line, allowing the user to terminate the process
-- at anytime. Also, when a user terminates a process, keep track of the 
-- list of addresses completed so far somewhere. 
-- Load this cache to ensure that the same address is not processed 
-- twice.


newtype ProcessedAddressCache = ProcessedAddressCache {addressSet :: Set[Address]}
-- CLI <ipcPath> <inputFileName> <outputFileName>
-- ipcPath is the domain socket path that the node opens.
-- inputFileName contains the list of addresses to work with.
-- outputFileName is the location of a cache to keep track
-- of all the addresses processes till now. This can get tricky, 
-- if this information needs to be shared with other processes.
-- We will use zya and setup a database in that case.
parseArgs :: [String] -> IO [(String, String)]
parseArgs argumentList = 
  case argumentList of 
    [ipcPath, inputFileName, outputFileName] -> do 
      let background = True -- run the process in background, the server wont return a status.
      startGethNode ipcPath background 
      --wait a little
      startProcess ipcPath inputFileName outputFileName
      shutdownGethNode ipcPath
      return $ [show `fmap` ("Success", ipcPath)]        
    _ -> putStrLn(help) >> return [("Help" , help)]


help = "CLI <ipcPath> <inputFileName> <outputFileName> . " 
        <> "If you encounter issues, check if geth exists on the node. "
        <> "Geth is what we use, though it can work with other flavors, not tested."
        <> "Geth can be downloaded from the go-ethereum github."
        <> "Follow the instructions there to install geth. Then run the program."

type IPCPath = FilePath
startGethNode :: IPCPath -> Bool -> IO Bool
startGethNode = undefined 

-- The process of reading a file, 
startProcess :: IPCPath -> FilePath -> FilePath -> IO [(IPCPath, FilePath, FilePath)]
startProcess = undefined

-- do a ps for the geth command line using the file path, there
-- could be other nodes running on same host for the current user.
-- stop the geth node that was started on this file path. 
-- the file path should be unique across many runs to prevent
-- these nodes or sockets to be lying around.
shutdownGethNode :: IPCPath -> IO Bool
shutdownGethNode = undefined

entryPoint :: IO [(String, String)]
entryPoint = getArgs >>= parseArgs
