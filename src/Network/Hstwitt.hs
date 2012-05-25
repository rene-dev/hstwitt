import Web.Authenticate.OAuth
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Conduit
import qualified Data.Map as Map
import System.IO
import System.IO.Unsafe
import System.Directory

oauth = newOAuth    { oauthServerName = "Twitter"
                    , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
                    , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
                    , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
                    , oauthSignatureMethod = HMACSHA1
                    , oauthConsumerKey = B.pack "Re5kMyM5OwWSDLIJmn0dw"
                    , oauthConsumerSecret = B.pack "fbXJ16L3KbeSujs6olCj5Nm3MGlTigJhREcfnMSC0"
                    , oauthCallback = Nothing
                    , oauthRealm = Nothing
                    , oauthVersion =  OAuth10a
    }

configfile = unsafePerformIO getHomeDirectory ++ "/.hstwitt"

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

