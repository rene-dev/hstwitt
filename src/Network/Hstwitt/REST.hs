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
import Network.HTTP.Types
import System.IO
import qualified Control.Monad.Trans.Resource as C



{-statuses_home_timeline :: IO
statuses_mentions,
statuses_retweeted_by_me,
statuses_retweeted_to_me,
statuses_retweets_of_me,
statuses_user_timeline,
statuses_retweeted_to_user,
statuses_retweeted_by_user -}

data ProfileImageSize = SizeBigger | SizeNormal | SizeMini | SizeOriginal

getUserProfileImageUrl :: String -> ProfileImageSize -> IO (Request a)
getUserProfileImageUrl user size = do
                let userquery = (B.pack "screen_name", Just $ B.pack user)
                let sizequery = (B.pack "size", Just $ case size of
                        SizeBigger -> B.pack "bigger"
                        SizeNormal-> B.pack "normal"
                        SizeMini -> B.pack "mini"
                        SizeOriginal -> B.pack "original"
                        )
                let query = renderQuery False [userquery, sizequery]
                url <- (parseUrl $ "http://api.twitter.com/1/users/profile_image?screen_name=" ++ user) 
                let queryurl = url {queryString = query}
                return queryurl

                

signedHttp :: MonadIO m => Credential -> Request (C.ResourceT IO) -> m L.ByteString
signedHttp cred request = liftIO $ withManager $ \man -> do
--        liftIO $ putStrLn url
--        url' <- liftIO $ parseUrl url
        request' <- signOAuth oauth cred request
        fmap responseBody $ httpLbs request' man


getNewHomeTimeline :: Conf -> IO (Maybe Tweets)
getNewHomeTimeline conf = do
        since_id_str_file <- getSinceIdStrconfigfile
        maybe_since_id_str <- readConf since_id_str_file
        putStrLn $ show maybe_since_id_str
        getHomeTimeline conf maybe_since_id_str

getHomeTimeline :: Conf -> Maybe Conf -> IO (Maybe Tweets)
getHomeTimeline conf since_id_str = do
        let cred = Credential $ Map.toList conf        
        request <- parseUrl $ "http://api.twitter.com/1/statuses/home_timeline.json" 
        let queryrequest = if isNothing since_id_str then request else request {queryString = renderQuery False [(B.pack "since_id", Just (fromJust since_id_str Map.! (B.pack "since_id")))]}
        jsontimeline <- signedHttp cred $ queryrequest
        let timeline = fromJust $ decode jsontimeline  :: Tweets
        case timeline of
            a@(x:xs) -> do
                putStrLn $ "Neue Tweets: " ++ (show $ length $ a)
                let new_since_id_str = tid_str $ head a
                let old_since_id_str = if isNothing since_id_str then "" else (B.unpack $ fromJust since_id_str Map.! (B.pack "since_id"))
                if old_since_id_str == new_since_id_str then do
                    putStrLn "Da is ja nix passiert... dennoch hat der runtergeladen... ist ja ulkig"
                    else return ()
                let new_since_id_str_conf =  Map.fromList [(B.pack "since_id", B.pack new_since_id_str)]
                since_id_str_file <- getSinceIdStrconfigfile    
                writeConf since_id_str_file new_since_id_str_conf
                return $ Just timeline
            otherwise -> return Nothing

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
