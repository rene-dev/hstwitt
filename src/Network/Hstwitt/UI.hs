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
        timeoutAddFull (yield >> return True)
                         priorityDefaultIdle 50
        uiFile <- getDataFileName "UI.glade"
        Just xml <- xmlNew uiFile
        timeline <- getHomeTimeline conf Nothing
        window   <- xmlGetWidget xml castToWindow "window_MainWindow"
        tweetsBox <- xmlGetWidget xml castToVBox "vbox_tweets"
        tweetsViewPort <- xmlGetWidget xml castToViewport "viewport_tweets"
        imageTweet <- xmlGetWidget xml castToImage "image_tweet"
        textviewTweet <- xmlGetWidget xml castToTextView "textview_tweet"
        buttonTweet <- xmlGetWidget xml castToButton "button_tweet"
        let screen_name = getFromConfS "screen_name" conf
        image <- getImg (getUserProfileImageUrl screen_name Nothing) screen_name
        imageSetFromFile imageTweet image 
        mapM (addTweet tweetsBox) $ reverse timeline        
        scrollDown tweetsViewPort
        onClicked buttonTweet $ sendTweet conf textviewTweet
        forkIO $ updateLoop conf tweetsBox
        onDestroy window mainQuit
        widgetShowAll window
        mainGUI

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
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

scrollDown :: Viewport -> IO ()
scrollDown sw = do
    swva <- viewportGetVAdjustment sw
    down <- adjustmentGetUpper swva
    adjustmentSetValue swva down


sendPin :: (a -> String -> IO()) -> a -> Entry -> IO ()
sendPin auth credentials pinEntry = do
    pin <- entryGetText pinEntry
    auth credentials pin
    

updateLoop :: Conf -> VBox -> IO ()
updateLoop conf tweetsBox = do
    threadDelay 20000000
    putStrLn "Update"
    ntimeline <- getNewHomeTimeline conf
    mapM (addTweet tweetsBox) $ reverse ntimeline
    widgetQueueDraw tweetsBox
    widgetHide tweetsBox
    widgetShowAll tweetsBox
    updateLoop conf tweetsBox

sendTweet :: Conf -> TextView -> IO ()
sendTweet conf tw = do
    buffer <- textViewGetBuffer tw
    start <- textBufferGetStartIter buffer
    end <- textBufferGetEndIter buffer
    tweet <- textBufferGetText buffer start end False
    textBufferDelete buffer start end
    thread <- forkIO $ updateTweets conf tweet
    return ()


getImg :: String -> String -> IO FilePath
getImg url img = do
        cachedir <- getCachedir
        fileExists <- doesFileExist (cachedir ++ img)
        if not fileExists then do
            request <- parseUrl $ url
            liftIO $ withManager $ \manager -> do
                      response <- http request manager
                      (responseBody response) C.$$ sinkFile $ cachedir ++ img
          else
            return ()
        return $ cachedir ++ img


getTweetImg :: Tweet -> IO FilePath
getTweetImg t = getImg (tuprofile_image_url $ tuser $ t) (tuscreen_name $ tuser t)

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

