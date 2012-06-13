module Network.Hstwitt.UI (
    createGUI,
    addTweet,
    initGUI
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
import Paths_Hstwitt

createGUI conf = do
        uiFile <- getDataFileName "UI.glade"
        Just xml <- xmlNew uiFile
--        putStrLn uiFile
        timeline <- getHomeTimeline conf "foo"
        window   <- xmlGetWidget xml castToWindow "window_MainWindow"
        tweetsBox <- xmlGetWidget xml castToVBox "vbox_tweets"
        imageTweet <- xmlGetWidget xml castToImage "image_tweet"
        textviewTweet <- xmlGetWidget xml castToTextView "textview_tweet"
        buttonTweet <- xmlGetWidget xml castToButton "button_tweet"
        let screen_name = getFromConfS "screen_name" conf
        image <- getImg (getUserProfileImageUrl screen_name Nothing) screen_name
        imageSetFromFile imageTweet image 
        mapM (addTweet tweetsBox)  timeline
        onClicked buttonTweet $ sendTweet conf textviewTweet
        onDestroy window mainQuit
        widgetShowAll window
        mainGUI

sendTweet :: Conf -> TextView -> IO ()
sendTweet conf tw = do
    buffer <- textViewGetBuffer tw
    start <- textBufferGetStartIter buffer
    end <- textBufferGetEndIter buffer
    tweet <- textBufferGetText buffer start end False
    textBufferDelete start end
    updateTweets conf tweet


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

