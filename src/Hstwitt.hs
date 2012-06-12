import Web.Authenticate.OAuth
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import Control.Concurrent
import qualified Data.Map as Map
import System.IO
import System.Directory
import Network.Hstwitt.Const
import Network.Hstwitt.Conf
import Network.Hstwitt.Types
import Control.Exception
import Data.Maybe
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource
import Network.Hstwitt.UI

main = do
    hSetBuffering stdout NoBuffering -- fixes problems with the output
    configfile <- getOauthconfigfile
    conf <- readConf configfile   
    if isNothing conf then 
		auth 
	 else  do 
                initGUI
                createGUI $ fst $ fromJust conf
                putStrLn "Foobar"


	--	tweet $ fst $ fromJust conf

{-tweet conf = do
	let cred = Credential $ Map.toList conf
	jsontimeline <- signedHttp cred twittertest
	let timeline = fromJust $ decode jsontimeline  :: Tweets
	mapM printtweet $ reverse timeline
	return ()

colorize :: String -> String -> String
colorize c s = "\ESC[" ++ c ++ "m" ++ s ++ "\ESC[m"


printtweet :: Tweet -> IO ()
printtweet t = putStrLn $ 
		colorize "1;30" (tid_str $ t) ++ " " ++
		colorize "1" (tcreated_at $ t) ++ "\n\t" ++
		colorize "1;32" (tuscreen_name $ tuser t) ++": " ++
		ttext t -}

auth = do    
    putStrLn "Keine Configdatei gefunden oder Configdatei fehlerhaft, bitte den Link klicken"
    credentials <- withManager $ \manager -> getTemporaryCredential oauth manager
    putStrLn $ authorizeUrl oauth credentials
    putStr "Bitte PIN eingeben: "
    pin <- getLine
    let auth = injectVerifier (B.pack pin) credentials
    accessToken <- withManager $ \manager -> getAccessToken oauth auth manager
    cachedir <- getCachedir
    configfile <- getOauthconfigfile
    appdir <- getAppUserDataDirectory "hstwitt"
    createDirectoryIfMissing True appdir
    createDirectoryIfMissing True cachedir
    writeConf configfile $ Map.fromList $ unCredential accessToken
    putStrLn $ "Config in " ++ configfile ++ " gespeichert"

