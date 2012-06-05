module Network.Hstwitt.Const (
    oauth,
    getOauthconfigfile,
    getCachedir
) where

import Web.Authenticate.OAuth
import System.Directory
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as B

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


getOauthconfigfile = do
        appdir <- getAppUserDataDirectory "hstwitt"
        return $ appdir ++ "/oauth.conf"

getCachedir = do
        appdir <- getAppUserDataDirectory "hstwitt"
        return $ appdir ++ "/cache/"
