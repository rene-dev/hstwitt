{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Hstwitt.Types (
      Tweets (Tweets,ttweets)
    , Tweet ( Tweet
            , tcreated_at
            , tid
            , tid_str
            , ttext
            , tsource
            , ttruncated
            , tin_reply_to_status_id
            , tin_reply_to_status_id_str
            , tin_reply_to_user_id
            , tin_reply_to_user_id_str
            , tin_reply_to_screen_name
            , tuser
--            , tgeo
--            , tplace
--          , tcontributors
            , tretweet_count
            , tfavorited
            , tretweeted
            )
                    
    , TUser ( TUser
            , tuid
            , tuid_str
            , tuname
            , tuscreen_name
            , tulocation
            , tudescription
            , tuurl
            , tuprotected
            , tufollowers_count
            , tufriends_count
            , tulisted_count
            , tucreated_at
            , tufavourites_count
            , tuutc_offset
            , tutime_zone
            , tugeo_enabled
            , tuverified
            , tustatuses_count
            , tulang
            , tucontributors_enabled
            , tuis_translator
            , tuprofile_background_color
            , tuprofile_background_image_url
            , tuprofile_background_image_url_https
            , tuprofile_image_url
            , tuprofile_link_color
            , tuprofile_sidebar_border_color
            , tuprofile_sidebar_fill_color
            , tuprofile_text_color
            , tuprofile_use_background_image
            , tushow_all_inline_media
            , tudefault_profile
            , tudefault_profile_image
            , tufollowing
            )
	,TestJS (TestJS, test1, test2)
    )
where

import Data.Aeson
--import Data.Functor
import Control.Applicative
import Control.Monad
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM


data TestJS = TestJS {
		  test1 :: String
		, test2 :: Int
} deriving Show

instance FromJSON TestJS where
	parseJSON (Object o) = do
			test1' <- o .: "test1"
			test2' <- o .:? "test2" .!= 12
			return $ TestJS { test1 = test1',  test2 = test2'}
	parseJSON _ = mzero
		
			

data Tweets = Tweets { ttweets :: [Tweet] } deriving Show
instance FromJSON Tweets where
	parseJSON (Array a) = do
			ttweets' <- mapM parseJSON (V.toList a)
			return $ Tweets {ttweets = ttweets'}
	parseJSON _ = mzero

data Tweet = Tweet  { tcreated_at :: String
                    , tid :: Int
                    , tid_str :: String
                    , ttext :: String
                    , tsource :: String
                    , ttruncated :: Bool
                    , tin_reply_to_status_id :: Maybe Int
                    , tin_reply_to_status_id_str :: Maybe String
                    , tin_reply_to_user_id :: Maybe Int
                    , tin_reply_to_user_id_str :: Maybe String
                    , tin_reply_to_screen_name :: Maybe String
                    , tuser :: TUser
--                    , tgeo :: String
--                    , tplace :: Maybe String
--                    , tcontributors :: undefined
                    , tretweet_count :: Int
                    , tfavorited :: Maybe Bool
                    , tretweeted :: Bool
                    } deriving Show

instance FromJSON Tweet where
	parseJSON (Object o) = do
		tcreated_at' <- o .: "created_at"
                tid' <- o .: "id"
                tid_str' <- o .: "id_str"
                ttext' <- o .: "text"
                tsource' <- o .: "source"
                ttruncated' <- o .: "truncated"
                tin_reply_to_status_id' <- o .:? "in_reply_to_status_id"
                tin_reply_to_status_id_str' <- o .:? "in_reply_to_status_id_str"
                tin_reply_to_user_id' <- o .:? "in_reply_to_user_id"
                tin_reply_to_user_id_str' <- o .:? "in_reply_to_user_id_str"
                tin_reply_to_screen_name' <- o .:? "in_reply_to_screen_name"
                tuser' <- o .: "user "
--                tgeo' <- o .: "geo"
--                tplace'<- o .:? "place"
                tretweet_count' <- o .: "retweet_count"
                tfavorited' <- o .:? "favorited"
                tretweeted' <- o .: "retweeted"
		return $ Tweet 	{ tcreated_at = tcreated_at'
				, tid = tid'
				, tid_str = tid_str'
				, ttext = ttext'
				, tsource = tsource'
				, ttruncated = ttruncated'
				, tuser = tuser'
				, tin_reply_to_status_id = tin_reply_to_status_id'
				, tin_reply_to_status_id_str = tin_reply_to_status_id_str'
				, tin_reply_to_user_id = tin_reply_to_user_id'
				, tin_reply_to_user_id_str = tin_reply_to_user_id_str'
				, tin_reply_to_screen_name = tin_reply_to_screen_name'
--				, tgeo = tgeo'
--				, tplace = tplace'
				, tretweet_count = tretweet_count'
				, tfavorited = tfavorited'
				, tretweeted = tretweeted'
				}
	parseJSON _ = mzero



data TUser = TUser  { tuid :: Int
                    , tuid_str :: String
                    , tuname :: String
                    , tuscreen_name :: String
                    , tulocation :: Maybe String
                    , tudescription :: Maybe String
                    , tuurl :: Maybe String
                    , tuprotected :: Bool
                    , tufollowers_count :: Int
                    , tufriends_count :: Int
                    , tulisted_count :: Int
                    , tucreated_at :: String
                    , tufavourites_count :: Int
                    , tuutc_offset :: Maybe Int
                    , tutime_zone :: Maybe String
                    , tugeo_enabled :: Bool
                    , tuverified :: Bool
                    , tustatuses_count :: Int
                    , tulang :: String
                    , tucontributors_enabled :: Bool
                    , tuis_translator :: Bool
                    , tuprofile_background_color :: String
                    , tuprofile_background_image_url :: String
                    , tuprofile_background_image_url_https :: String
                    , tuprofile_image_url :: String
                    , tuprofile_link_color :: String
                    , tuprofile_sidebar_border_color :: String
                    , tuprofile_sidebar_fill_color :: String
                    , tuprofile_text_color :: String
                    , tuprofile_use_background_image :: Bool
                    , tushow_all_inline_media :: Bool
                    , tudefault_profile :: Bool
                    , tudefault_profile_image :: Bool
                    , tufollowing :: Maybe Bool
--                    , tufollow_request_sent :: undefined
--                    , tunotifications :: undefined
                    } deriving Show
instance FromJSON TUser where
	parseJSON (Object o) = do
		tuid' <- o .: "id"
                tuid_str' <- o .: "id_str"
                tuname' <- o .: "name"
                tuscreen_name' <- o .: "screen_name"
                tulocation' <- o .:? "location"
                tudescription' <- o .:? "description"
                tuurl' <- o .:? "url"
                tuprotected' <- o .: "protected"
                tufollowers_count' <- o .: "followers_count"
                tufriends_count' <- o .: "friends_count"
                tulisted_count' <- o .: "listed_count"
                tucreated_at' <- o .: "created_at"
                tufavourites_count' <- o .: "favourites_count"
                tuutc_offset' <- o .:? "utc_offset"
                tutime_zone' <- o .:? "time_zone"
                tugeo_enabled' <- o .: "geo_enabled"
                tuverified' <- o .: "verified"
                tustatuses_count' <- o .: "statuses_count"
                tulang' <- o .: "lang"
                tucontributors_enabled' <- o .: "contributors_enabled"
                tuis_translator' <- o .: "is_translator"
                tuprofile_background_color' <- o .: "profile_background_color"
                tuprofile_background_image_url' <- o .: "profile_background_image_url"
                tuprofile_background_image_url_https' <- o .: "profile_background_image_url_https"
                tuprofile_image_url' <- o .: "profile_image_url"
                tuprofile_link_color' <- o .: "profile_link_color"
                tuprofile_sidebar_border_color' <- o .: "profile_sidebar_border_color"
                tuprofile_sidebar_fill_color' <- o .: "profile_sidebar_fill_color"
                tuprofile_text_color' <- o .: "profile_text_color"
                tuprofile_use_background_image' <- o .: "profile_use_background_image"
                tushow_all_inline_media' <- o .: "show_all_inline_media"
                tudefault_profile' <- o .: "default_profile"
                tudefault_profile_image' <- o .: "default_profile_image"
                tufollowing' <- o .:? "following"
		return $ TUser 	{ tuid = tuid'
                		, tuid_str = tuid_str'
		                , tuname = tuname'
                		, tuscreen_name = tuscreen_name'
		                , tulocation = tulocation'
		                , tudescription = tudescription'
		                , tuurl = tuurl'
		                , tuprotected = tuprotected'
		                , tufollowers_count = tufollowers_count'
		                , tufriends_count = tufriends_count'
		                , tulisted_count = tulisted_count'
		                , tucreated_at = tucreated_at'
		                , tufavourites_count = tufavourites_count'
		                , tuutc_offset = tuutc_offset'
		                , tutime_zone = tutime_zone'
		                , tugeo_enabled = tugeo_enabled'
		                , tuverified = tuverified'
		                , tustatuses_count = tustatuses_count'
		                , tulang = tulang'
		                , tucontributors_enabled = tucontributors_enabled'
		                , tuis_translator = tuis_translator'
		                , tuprofile_background_color = tuprofile_background_color'
		                , tuprofile_background_image_url = tuprofile_background_image_url'
		                , tuprofile_background_image_url_https = tuprofile_background_image_url_https'
		                , tuprofile_image_url = tuprofile_image_url'
		                , tuprofile_link_color = tuprofile_link_color'
		                , tuprofile_sidebar_border_color = tuprofile_sidebar_border_color'
		                , tuprofile_sidebar_fill_color = tuprofile_sidebar_fill_color'
		                , tuprofile_text_color = tuprofile_text_color'
		                , tuprofile_use_background_image = tuprofile_use_background_image'
		                , tushow_all_inline_media = tushow_all_inline_media'
		                , tudefault_profile = tudefault_profile'
		                , tudefault_profile_image = tudefault_profile_image'
		                , tufollowing = tufollowing'
				}
