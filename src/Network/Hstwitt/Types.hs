{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Hstwitt.Types (
      Tweets    ( Tweets
                , ttweets
                )
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
            , tgeo
            , tplace
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
    )
where

import GHC.Generics
import Data.Aeson

data Tweets = Tweets { ttweets :: [Tweets]} deriving Generic
instance FromJSON Tweets
instance ToJSON Tweets

data Tweet = Tweet  { tcreated_at :: String
                    , tid :: Int
                    , tid_str :: String
                    , ttext :: String
                    , tsource :: String
                    , ttruncated :: Bool
                    , tin_reply_to_status_id :: Int
                    , tin_reply_to_status_id_str :: String
                    , tin_reply_to_user_id :: Int
                    , tin_reply_to_user_id_str :: String
                    , tin_reply_to_screen_name :: String
                    , tuser :: TUser
                    , tgeo :: String
                    , tplace :: String
--                    , tcontributors :: undefined
                    , tretweet_count :: Int
                    , tfavorited :: Bool
                    , tretweeted :: Bool
                    } deriving Generic
instance FromJSON Tweet
instance ToJSON Tweet

data TUser = TUser  { tuid :: Int
                    , tuid_str :: String
                    , tuname :: String
                    , tuscreen_name :: String
                    , tulocation :: String
                    , tudescription :: String
                    , tuurl :: String
                    , tuprotected :: Bool
                    , tufollowers_count :: Int
                    , tufriends_count :: Int
                    , tulisted_count :: Int
                    , tucreated_at :: String
                    , tufavourites_count :: Int
                    , tuutc_offset :: Int
                    , tutime_zone :: String
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
                    , tufollowing :: Bool
--                    , tufollow_request_sent :: undefined
--                    , tunotifications :: undefined
                    } deriving Generic
instance FromJSON TUser
instance ToJSON TUser
