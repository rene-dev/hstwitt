import Web.Authenticate.OAuth
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Conduit
import qualified Data.Map as Map
import System.IO
import System.Directory
import Network.Hstwitt.Const
import Network.Hstwitt.Conf

main = do
    configured <- doesFileExist configfile
    if configured then 
		tweet 
	 else 
		auth

tweet = do
    conf <- readConf configfile
    putStrLn $ show conf

auth = do    
    putStrLn "Keine Configdatei gefunden, bitte den Link klicken"
    credentials <- withManager $ \manager -> getTemporaryCredential oauth manager
    putStrLn $ authorizeUrl oauth credentials
    putStr "Bitte PIN eingeben:"
    pin <- getLine
    let auth = injectVerifier (B.pack pin) credentials
    accessToken <- withManager $ \manager -> getAccessToken oauth auth manager
    writeConf configfile $ Map.fromList $ unCredential accessToken
    putStrLn $ "Config in " ++ configfile ++ " gespeichert"

