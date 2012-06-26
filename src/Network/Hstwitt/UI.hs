module Network.Hstwitt.UI (
    createGUI,
    addTweet,
    createAuthGUI
)
where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Data.Maybe
import System.Directory
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Conduit.Binary (sinkFile)
import qualified Data.Conduit as C
import Network.HTTP.Conduit
import Network.Hstwitt.REST
import Network.Hstwitt.Types
import Network.Hstwitt.Conf
import Network.Hstwitt.Const
import Control.Concurrent
import Paths_Hstwitt

createGUI conf = do
        initGUI
        uiFile <- getDataFileName "UI.glade"
        Just xml <- xmlNew uiFile
        window   <- xmlGetWidget xml castToWindow "window_MainWindow"
        tweetsBox <- xmlGetWidget xml castToVBox "vbox_tweets"
        tweetsViewport <- xmlGetWidget xml castToViewport "viewport_tweets"
        imageTweet <- xmlGetWidget xml castToImage "image_tweet"
        textviewTweet <- xmlGetWidget xml castToTextView "textview_tweet"
        buttonTweet <- xmlGetWidget xml castToButton "button_tweet"
        let screen_name = getFromConfS "screen_name" conf
        profileUrl <- getUserProfileImageUrl screen_name SizeNormal
        image <- getImg profileUrl screen_name
        imageSetFromFile imageTweet image 
        timeline <- getHomeTimeline conf Nothing
        case timeline of
            Just jtimeline -> mapM (addTweet tweetsBox) $ reverse jtimeline
            Nothing -> return [()]
        on buttonTweet buttonActivated $ sendTweet conf textviewTweet
        on textviewTweet keyPressEvent $ enterSendTweet conf textviewTweet
--        on tweetsBox add $ \_ -> scrollDown tweetsViewport
--        after tweetsBox add $ \_ -> scrollDown tweetsViewport
        timeoutAdd (postGUIAsync (update conf tweetsViewport) >>= \_ -> return True) 10000
        onDestroy window mainQuit
        widgetShowAll window
        adjustment <- viewportGetVAdjustment tweetsViewport
        scrollDown True adjustment
        mainGUI

--enterSendTweet :: EventM EKey Bool
enterSendTweet conf tv =  do 
        tryEvent $ do
            "Return" <- eventKeyName
            [Control] <- eventModifier
            liftIO $ sendTweet conf tv

createAuthGUI :: IO (String, a) -> (a -> String -> IO ()) -> IO ()
createAuthGUI getauthcode auth= do
    initGUI
    uiFile <- getDataFileName "Auth.glade"
    builder <- builderNew
    builderAddFromFile builder uiFile
    pinLinkButton <- builderGetObject builder castToLinkButton "linkbutton_PIN"
    window <- builderGetObject builder castToWindow "window_oauth"
    okButton <- builderGetObject builder castToButton "button_OK"
    pinEntry <- builderGetObject builder castToEntry "entry_PIN"
    (authcode, credentials) <- getauthcode
    buttonSetLabel pinLinkButton "Link"
    set pinLinkButton [linkButtonURI := authcode]
    onClicked okButton $ do 
        sendPin auth credentials pinEntry
        widgetHide window
    onDestroy window $ widgetDestroy window
    widgetShowAll window
    mainGUI

getIsScrolledDown :: Viewport -> IO Bool
getIsScrolledDown viewport = do
    adjustment <- viewportGetVAdjustment viewport
    value <- adjustmentGetValue adjustment
    upper <- adjustmentGetUpper adjustment
    page_size <- adjustmentGetPageSize adjustment
    putStrLn $ (show value) ++ show (upper - page_size)
    return (value == (upper - page_size))


scrollDown :: Bool -> Adjustment -> IO ()
scrollDown doIt adjustment = if doIt then do
        putStrLn "Downdowndown it goes"
--        adjustment <- viewportGetVAdjustment viewport
        upper <- adjustmentGetUpper adjustment
        page_size <- adjustmentGetPageSize adjustment
        adjustmentSetValue adjustment $ (upper - page_size) + 1 
        else do
            putStrLn "Ich will nicht"

sendPin :: (a -> String -> IO()) -> a -> Entry -> IO ()
sendPin auth credentials pinEntry = do
    pin <- entryGetText pinEntry
    auth credentials pin
    

update :: Conf -> Viewport -> IO ()
update conf tweetsViewport = do    
    Just tweetsWidget <- binGetChild tweetsViewport :: IO (Maybe Widget)
    wasScrolledDown <- getIsScrolledDown tweetsViewport
    let tweetsBox = castToVBox tweetsWidget
    adjustment <- viewportGetVAdjustment tweetsViewport
    signal <- afterAdjChanged adjustment $ scrollDown wasScrolledDown adjustment
--    signal <- afterAdjChanged adjustment $ scrollDown wasScrolledDown adjustment
    putStrLn "Update"
    ntimeline <- getNewHomeTimeline conf
    case ntimeline of
        Just jtimeline -> do
            mapM (addTweet tweetsBox) $ reverse jtimeline
--            widgetQueueDraw tweetsBox
            widgetHide tweetsBox
            widgetShowAll tweetsBox
--            scrollDown wasScrolledDown tweetsViewport
--            if wasScrolledDown then scrollDown tweetsViewport else return ()
        Nothing -> do
--            putStrLn "Nichts zu tun"
            return ()
--    if wasScrolledDown then
--        signalDisconnect signal
--     else
--        return ()
--    updateLoop conf tweetsViewport

sendTweet :: Conf -> TextView -> IO ()
sendTweet conf tw = do    
    buffer <- textViewGetBuffer tw
    start <- textBufferGetStartIter buffer
    end <- textBufferGetEndIter buffer
    tweet <- textBufferGetText buffer start end False
    textBufferDelete buffer start end
    postGUIAsync $ updateTweets conf tweet
    return ()


getImg :: Request (C.ResourceT IO) -> String -> IO FilePath
getImg request img = do
--        putStrLn url
        cachedir <- getCachedir
        fileExists <- doesFileExist (cachedir ++ img)
        if not fileExists then do
--            request <- parseUrl $ url
--            putStrLn $ B.unpack $ queryString request
            liftIO $ withManager $ \manager -> do
                      response <- http request manager
                      (responseBody response) C.$$ sinkFile $ cachedir ++ img
          else
            return ()
        return $ cachedir ++ img


getTweetImg :: Tweet -> IO FilePath
getTweetImg t = do
    request <- parseUrl $ tuprofile_image_url $ tuser $ t
    getImg request (tuscreen_name $ tuser t)


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
    boxPackStart vBox tweetbox PackNatural 0
--    widgetShowAll vBox
--    putStrLn $ tuscreen_name $ tuser tweet

tweet2textBuffer :: Tweet -> TextTagTable-> IO TextBuffer
tweet2textBuffer t tagtable = do
                        buffer <- textBufferNew $ Just tagtable

                        add2textBuffer buffer "Time" (Just "jright") $ tcreated_at t ++ "\n"
                        add2textBuffer buffer "Name" (Just "Bold") $ (tuscreen_name $ tuser t) ++ "\n"
                        add2textBuffer buffer "Text" Nothing $ ttext t
--                        putStrLn $ ttext t --DEBUG
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

