import Web.Authenticate.OAuth
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Conduit

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


main = do
    credentials <- withManager $ \manager -> getTemporaryCredential oauth manager
    putStrLn $ authorizeUrl oauth credentials
    pin <- getLine
    let auth = injectVerifier (B.pack pin) credentials
    accessToken <- withManager $ \manager -> getAccessToken oauth auth manager
    putStrLn $ show accessToken

