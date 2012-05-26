module Network.Hstwitt.Conf (
    Conf,
    newConf,
    addToConf,
    delFromConf,
    writeConf,
    readConf
    ) where

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import System.IO

type Conf = Map.Map B.ByteString B.ByteString

newConf :: Conf
newConf = Map.empty

addToConf :: B.ByteString -> B.ByteString -> Conf -> Conf
addToConf = Map.insert

delFromConf :: B.ByteString -> Conf -> Conf
delFromConf = Map.delete

writeConf :: String -> Conf -> IO ()
writeConf filename = writeFile filename . show

readConf :: String -> IO (Conf)
readConf filename = fmap read (readFile filename)


