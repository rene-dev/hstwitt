module Network.Hstwitt.Conf (
    Conf,
    newConf,
    addToConf,
    delFromConf,
    getFromConf,
    getFromConfS,
    writeConf,
    readConf
    ) where

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import System.IO
import Control.Exception

type Conf = Map.Map B.ByteString B.ByteString

newConf :: Conf
newConf = Map.empty

getFromConf :: B.ByteString -> Conf -> B.ByteString
getFromConf key conf = conf Map.! key

getFromConfS :: String -> Conf -> String
getFromConfS key conf = B.unpack $ getFromConf (B.pack key) conf

addToConf :: B.ByteString -> B.ByteString -> Conf -> Conf
addToConf = Map.insert

delFromConf :: B.ByteString -> Conf -> Conf
delFromConf = Map.delete

writeConf :: String -> Conf -> IO ()
writeConf filename = writeFile filename . show


-- Versucht die configdatei zu lesen. Wenn etwas schief läuft, wird Nothing zurück gegeben
readConf :: String -> IO (Maybe (Conf, String))
readConf filename = do
    content <- try $ readFile filename :: IO (Either IOException String)
    return $ case content of
        Left _ -> Nothing
        Right conf -> listToMaybe $ reads conf


