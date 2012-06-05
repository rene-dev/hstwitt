import Web.Authenticate.OAuth
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import qualified Data.Conduit as C
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
import Graphics.UI.Gtk

twittertest = "http://api.twitter.com/1/statuses/home_timeline.json"

debugGetCred = do
    configfile <- getOauthconfigfile
    mconf <- readConf configfile
    let conf = fst $ fromJust mconf
    return $ Credential $ Map.toList conf

test = do
        configfile <- getOauthconfigfile
	mconf <- readConf configfile
	let conf = fst $ fromJust mconf
	signedHttp (Credential $ Map.toList conf) twittertest

debugreq :: IO (C.Source (ResourceT IO) B.ByteString)
debugreq = liftIO $ withManager $ \m -> do
        cred <- liftIO $ debugGetCred
        req <- liftIO $ parseUrl twittertest
        surl <- signOAuth oauth cred req
        fmap responseBody $ http surl m


signedHttp :: MonadIO m => Credential -> String -> m L.ByteString
signedHttp cred url = liftIO $ withManager $ \man -> do
        url' <- liftIO $ parseUrl url
        url'' <- signOAuth oauth cred url'
        fmap responseBody $ httpLbs url'' man

main = do
    hSetBuffering stdout NoBuffering -- fixes problems with the output
    configfile <- getOauthconfigfile
    conf <- readConf configfile   
    if isNothing conf then 
		auth 
	 else 
                createGUI $ fst $ fromJust conf
	--	tweet $ fst $ fromJust conf

createGUI conf = do
	let cred = Credential $ Map.toList conf
	jsontimeline <- signedHttp cred twittertest
	let timeline = fromJust $ decode jsontimeline  :: Tweets
        initGUI
        window <- windowNew
        vPaned <- vPanedNew
        inputField <- textViewNew
        tweetsScroll <- scrolledWindowNew Nothing Nothing
        tweetsBox <- vBoxNew False 5
        scrolledWindowAddWithViewport tweetsScroll tweetsBox
        set window [ containerChild := vPaned , windowTitle := "HsTwitt" ]
        mapM (addTweet tweetsBox)  timeline
        panedPack1 vPaned tweetsScroll True False
        panedPack2 vPaned inputField False False
        onDestroy window mainQuit
        widgetShowAll window
        mainGUI
    
getTweetImg :: Tweet -> IO FilePath
getTweetImg t = do
        cachedir <- getCachedir
        fileExists <- doesFileExist (cachedir ++ (tuscreen_name $ tuser t))
        if not fileExists then do
            request <- parseUrl $ tuprofile_image_url $ tuser $ t
            liftIO $ withManager $ \manager -> do
                      response <- http request manager
                      (responseBody response) C.$$ sinkFile $ cachedir ++ (tuscreen_name $ tuser t)
          else
            return ()
        return $ cachedir ++ (tuscreen_name $ tuser t)


addTweet :: VBox -> Tweet -> IO ()
addTweet vBox tweet = do
    tweetbox <- hBoxNew False 2
    tweetLabel <- textViewNew
    tweetimgpath <- getTweetImg tweet
    tweetimg <- imageNewFromFile tweetimgpath
    tagtable <- textTagTableNew
    bold <- textTagNew $ Just "Bold"
    set bold [ textTagFont := "Sans Bold 10" ]
    textTagTableAdd tagtable bold
    jright <- textTagNew $ Just "jright"
    set jright [ textTagJustification := JustifyRight, textTagFont := "Sans Italic 8"]
    textTagTableAdd tagtable jright
    textBuffer <- tweet2textBuffer tweet tagtable
    textViewSetBuffer tweetLabel textBuffer
    textViewSetEditable tweetLabel False
    textViewSetWrapMode tweetLabel WrapWord
    boxPackEnd tweetbox tweetLabel PackGrow 0
    boxPackEnd tweetbox tweetimg PackNatural 0
    boxPackEnd vBox tweetbox PackNatural 0


tweet2textBuffer :: Tweet -> TextTagTable-> IO TextBuffer
tweet2textBuffer t tagtable = do
                        buffer <- textBufferNew $ Just tagtable

			add2textBuffer buffer "Time" (Just "jright") $ tcreated_at t ++ "\n"
			add2textBuffer buffer "Name" (Just "Bold") $ (tuscreen_name $ tuser t) ++ "\n"
			add2textBuffer buffer "Text" Nothing $ ttext t
                        return buffer

add2textBuffer :: TextBuffer -> String -> Maybe String -> String -> IO ()
add2textBuffer buffer name tagname text = do
			is <- textBufferGetEndIter buffer
			ms <- textBufferCreateMark buffer (Just $ "Start"++name) is True
			textBufferInsert buffer is text
			ie <- textBufferGetEndIter buffer
			me <- textBufferCreateMark buffer (Just $ "End"++name) ie True
			if isJust tagname then do
				s <- textBufferGetIterAtMark buffer ms
				e <- textBufferGetIterAtMark buffer me
				textBufferApplyTagByName buffer (fromJust tagname) s e
				return ()
			  else return ()

			return ()
                        
tweet conf = do
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
		ttext t

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

