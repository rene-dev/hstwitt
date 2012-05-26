import Web.Authenticate.OAuth
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Conduit
import qualified Data.Map as Map
import System.IO
import System.Directory
import Network.Hstwitt.Const
import Network.Hstwitt.Conf
import Control.Exception
import Data.Maybe

main = do
    hSetBuffering stdin NoBuffering -- fixes problems with the output
    conf <- readConf configfile   
    if isNothing conf then 
		auth 
	 else 
		tweet $ fst $ fromJust conf

tweet conf = do
    putStrLn $ show $ Credential $ Map.toList conf

auth = do    
    putStrLn "Keine Configdatei gefunden oder Configdatei fehlerhaft, bitte den Link klicken"
    credentials <- withManager $ \manager -> getTemporaryCredential oauth manager
    putStrLn $ authorizeUrl oauth credentials
    putStr "Bitte PIN eingeben: "
    pin <- getLine
    let auth = injectVerifier (B.pack pin) credentials
    accessToken <- withManager $ \manager -> getAccessToken oauth auth manager
    writeConf configfile $ Map.fromList $ unCredential accessToken
    putStrLn $ "Config in " ++ configfile ++ " gespeichert"

