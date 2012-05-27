import Web.Authenticate.OAuth
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Network.HTTP.Conduit
import qualified Data.Map as Map
import System.IO
import System.Directory
import Network.Hstwitt.Const
import Network.Hstwitt.Conf
import Network.Hstwitt.Types
import Control.Exception
import Data.Maybe
import Control.Monad.IO.Class (MonadIO (liftIO))

twittertest = "http://api.twitter.com/1/statuses/home_timeline.json?count=2"

test = do
	mconf <- readConf configfile
	let conf = fst $ fromJust mconf
	return $ Credential $ Map.toList conf

signedHttp :: MonadIO m => Credential -> String -> m L.ByteString
signedHttp cred url = liftIO $ withManager $ \man -> do
        url' <- liftIO $ parseUrl url
        url'' <- signOAuth oauth cred url'
        fmap responseBody $ httpLbs url'' man

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

