module Network.Hstwitt.REST (
{-    statuses_home_timeline,
    statuses_mentions,
    statuses_retweeted_by_me,
    statuses_retweeted_to_me,
    statuses_retweets_of_me,
    statuses_user_timeline,
    statuses_retweeted_to_user,
    statuses_retweeted_by_user -}
    getHomeTimeline,
    getNewHomeTimeline,
    getUserProfileImageUrl,
    ProfileImageSize (SizeBigger, SizeNormal, SizeMini, SizeOriginal),
    updateTweets
    )
where
import Network.Hstwitt.Types
import Network.Hstwitt.Const
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class (MonadIO (liftIO))
import Network.Hstwitt.Conf
import qualified Data.Map as Map
import Data.Aeson
import Data.Maybe
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import System.IO



{-statuses_home_timeline :: IO
statuses_mentions,
statuses_retweeted_by_me,
statuses_retweeted_to_me,
statuses_retweets_of_me,
statuses_user_timeline,
statuses_retweeted_to_user,
statuses_retweeted_by_user -}

data ProfileImageSize = SizeBigger | SizeNormal | SizeMini | SizeOriginal

getUserProfileImageUrl :: String -> Maybe ProfileImageSize -> String
getUserProfileImageUrl user size = "https://api.twitter.com/1/users/profile_image?screen_name=" ++ user ++ case size of
                Just SizeBigger -> "&size=bigger"
                Just SizeNormal-> "&size=normal"
                Just SizeMini -> "&size=mini"
                Just SizeOriginal -> "&size=original"
                otherwise -> ""

signedHttp :: MonadIO m => Credential -> String -> m L.ByteString
signedHttp cred url = liftIO $ withManager $ \man -> do
        url' <- liftIO $ parseUrl url
        url'' <- signOAuth oauth cred url'
        fmap responseBody $ httpLbs url'' man


getNewHomeTimeline :: Conf -> IO Tweets
getNewHomeTimeline conf = do
        since_id_str_file <- getSinceIdStrconfigfile
        maybe_since_id_str <- readConf since_id_str_file
        putStrLn $ show maybe_since_id_str
        getHomeTimeline conf maybe_since_id_str

getHomeTimeline :: Conf -> Maybe Conf -> IO Tweets
getHomeTimeline conf since_id_str = do
        let cred = Credential $ Map.toList conf        
        putStrLn $  "http://api.twitter.com/1/statuses/home_timeline.json" ++ case since_id_str of
            Just a -> "?since_id=" ++ (B.unpack $ a Map.! (B.pack "since_id"))
            otherwise -> ""
        jsontimeline <- signedHttp cred $ "http://api.twitter.com/1/statuses/home_timeline.json" ++ case since_id_str of
            Just a -> "?since_id=" ++ (B.unpack $ a Map.! (B.pack "since_id"))
            otherwise -> ""
        let timeline = fromJust $ decode jsontimeline  :: Tweets
        let newestTweet = listToMaybe $ timeline
        case newestTweet of
            Just a -> do
                let new_since_id_str = tid_str a
                let new_since_id_str_conf =  Map.fromList [(B.pack "since_id", B.pack new_since_id_str)]
                since_id_str_file <- getSinceIdStrconfigfile    
                writeConf since_id_str_file new_since_id_str_conf
            otherwise -> return ()
        return timeline

updateTweets :: Conf -> String -> IO ()
updateTweets conf tweet = do
--        putStrLn  tweet --DEBUG
        let cred = Credential $ Map.toList conf
        url' <- parseUrl  "https://api.twitter.com/1/statuses/update.json"
        liftIO $ withManager $ \man -> do
            url'' <- signOAuth oauth cred $ urlEncodedBody [(B.pack "status", B.pack tweet)] url'
--        putStrLn $ show url''
            fmap responseBody $ httpLbs url'' man
        return ()
