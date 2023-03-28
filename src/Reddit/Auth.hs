module Reddit.Auth
  ( -- * Credentials
    -- 
    -- $credentials
    Credentials (..)
  , withCredentials
  , RedditEnv (..),
  )
where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Network.HTTP.Req

-- $credentials
--
-- This section describes the credentials needed for using the Reddit API as a
-- \'script\'.
--
-- In order to obtain these, you will need to:
--
--   1. Log in using the account you want to post or query Reddit as (this may be
--      a bot account, for example)
--   2. Navigate to <https://www.reddit.com/prefs/apps>
--   3. Click \'create another app\' at the bottom and select the \'script\' radio
--      button
--   4. Fill in the required details. For a personal script, the redirect URI is
--      not important, set it to anything you like.
--   5. Take note of the app ID (a string with random characters) and the secret
--      (the same but longer). These become, respectively,  @clientID@ and
--      @clientSecret@.
--   6. @username@ and @password@ are the Reddit login credentials of the account
--      you are using.
--
-- /Note:/ __Do NOT ever share your password or the client secret publicly!__
--
-- See also: <https://github.com/reddit-archive/reddit/wiki/OAuth2-Quick-Start-Example>

data Credentials = Credentials
  { username :: Text,
    password :: Text,
    clientID :: Text,
    clientSecret :: Text
  }
  deriving (Show)

-- | Access tokens

-- | The raw JSON representation of an access token as returned by Reddit.
data TokenInternal = TokenInternal
  { _access_token :: Text,
    _token_type :: Text,
    _expires_in :: Int,
    _scope :: Text
  }
  deriving (Show)

instance FromJSON TokenInternal where
  parseJSON (Object v) =
    TokenInternal
      <$> v
      .: "access_token"
      <*> v
      .: "token_type"
      <*> v
      .: "expires_in"
      <*> v
      .: "scope"

-- | A parsed value of a Reddit access token.
data Token = Token
  { token :: ByteString,
    token_type :: ByteString,
    expires_at :: UTCTime,
    scope :: ByteString
  }
  deriving (Show)

-- | Convert the raw JSON returned into a parsed token.
convertToken :: TokenInternal -> IO Token
convertToken t = do
  currentTime <- getCurrentTime
  let seconds = secondsToNominalDiffTime . realToFrac $ _expires_in t
  let expires_at = addUTCTime seconds currentTime
  pure $
    Token
      { token = TE.encodeUtf8 (_access_token t),
        token_type = TE.encodeUtf8 (_token_type t),
        scope = TE.encodeUtf8 (_scope t),
        expires_at = expires_at
      }

-- | Everything required to query the Reddit API.
data RedditEnv = RedditEnv
  { envToken :: ByteString,
    envTokenType :: ByteString,
    envTokenExpiresAt :: UTCTime,
    envTokenScope :: ByteString,
    envUserAgent :: ByteString
  }
  deriving (Show)

-- | Exchange user account credentials for a RedditEnv, which is required to run
-- all Reddit queries.
withCredentials :: Credentials
                -> Text  -- ^ Your user-agent. Reddit says you should use a unique and identifiable user-agent.
                -> IO RedditEnv
withCredentials creds userAgent = do
  tokenInternal <- runReq defaultHttpConfig $ do
    let uri = https "www.reddit.com" /: "api" /: "v1" /: "access_token"
    let body =
          ReqBodyUrlEnc
            ( "grant_type"
                =: ("password" :: Text)
                <> "username"
                =: username creds
                <> "password"
                =: password creds
            )
    let options = basicAuth (TE.encodeUtf8 (clientID creds)) (TE.encodeUtf8 (clientSecret creds))
    response <- req POST uri body lbsResponse options
    throwDecode (responseBody response)

  currentTime <- getCurrentTime
  let seconds = secondsToNominalDiffTime . realToFrac $ _expires_in tokenInternal
  let expires_at = addUTCTime seconds currentTime
  pure $
    RedditEnv
      { envToken = TE.encodeUtf8 (_access_token tokenInternal),
        envTokenType = TE.encodeUtf8 (_token_type tokenInternal),
        envTokenScope = TE.encodeUtf8 (_scope tokenInternal),
        envTokenExpiresAt = expires_at,
        envUserAgent = TE.encodeUtf8 userAgent
      }
