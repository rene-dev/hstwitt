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
writeConf filename conf = do 
--                putStrLn $ "Foo " ++ filename
                error <- try $ writeFile filename $ show conf :: IO (Either IOException ())
                case error of
                    Left _ -> putStrLn "Irgendwas ist beim schreiben schief gegangen.... egal"
                    Right _ -> return ()



-- Versucht die configdatei zu lesen. Wenn etwas schief läuft, wird Nothing zurück gegeben
readConf :: String -> IO (Maybe Conf)
readConf filename = do
    let readThisFile s h = do
        eof <- hIsEOF h
        if eof then return s else do
            content <- hGetLine h
            return $ s ++ "\n" ++ content
    content <- try $ withFile filename ReadMode $ readThisFile "" :: IO (Either IOException String)
    return $ case content of
        Left _ -> Nothing
        Right conf -> listToMaybe $ map fst $ reads conf

