{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Reddit where

import Control.Exception (Exception (..), throwIO)
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Network.HTTP.Req

data Credentials = Credentials
  { username :: Text,
    password :: Text,
    clientID :: Text,
    clientSecret :: Text
  }
  deriving (Show)

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

data Token = Token
  { token :: ByteString,
    token_type :: ByteString,
    expires_at :: UTCTime,
    scope :: ByteString
  }
  deriving (Show)

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
data Env = Env
  { envToken :: ByteString,
    envTokenType :: ByteString,
    envTokenExpiresAt :: UTCTime,
    envTokenScope :: ByteString,
    envUserAgent :: ByteString
  }
  deriving (Show)

-- | Get an Env value by supplying user account credentials. Only to be used for
-- quick scripts, etc.
authWithCredentials :: Credentials -> Text -> IO Env
authWithCredentials creds userAgent = do
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
    Env
      { envToken = TE.encodeUtf8 (_access_token tokenInternal),
        envTokenType = TE.encodeUtf8 (_token_type tokenInternal),
        envTokenScope = TE.encodeUtf8 (_scope tokenInternal),
        envTokenExpiresAt = expires_at,
        envUserAgent = TE.encodeUtf8 userAgent
      }

oauth :: Text
oauth = "oauth.reddit.com"

withUAToken :: Env -> Option 'Https
withUAToken env = header "user-agent" (envUserAgent env) <> oAuth2Bearer (envToken env)

data RedditException = TokenExpiredException deriving (Show)

instance Exception RedditException

-- | TODO: Don't check before using it, check the response headers instead to
-- determine if token has expired
checkTokenValidity :: ReaderT Env IO ()
checkTokenValidity = do
  expiryTime <- reader envTokenExpiresAt
  currentTime <- liftIO getCurrentTime
  when
    (nominalDiffTimeToSeconds (diffUTCTime currentTime expiryTime) > 0)
    (liftIO (throwIO TokenExpiredException))

-- | Get information about a user
user :: Text -> ReaderT Env IO ByteString
user username = do
  env <- ask
  checkTokenValidity
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "user" /: username /: "about"
    response <- req GET uri NoReqBody bsResponse (withUAToken env)
    pure $ responseBody response

data Timeframe = Hour | Day | Week | Month | Year | All

data SubredditSort = Hot | New | Random | Rising | Top Timeframe | Controversial Timeframe

-- | Get the first 25 posts on a subreddit
subreddit :: Text -> SubredditSort -> ReaderT Env IO ByteString
subreddit sr sort = do
  env <- ask
  checkTokenValidity
  let timeframe_text tf = case tf of
        Hour -> "hour" :: Text  -- the type checker needs help
        Day -> "day"
        Week -> "week"
        Month -> "month"
        Year -> "year"
        All -> "all"
  let (endpoint, tf_params) = case sort of
        Hot -> ("hot", mempty)
        New -> ("new", mempty)
        Random -> ("random", mempty)
        Rising -> ("rising", mempty)
        Top tf -> ("top", "t" =: timeframe_text tf)
        Controversial tf -> ("controversial", "t" =: timeframe_text tf)
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "r" /: sr /: endpoint
    response <- req GET uri NoReqBody bsResponse (withUAToken env <> tf_params)
    pure $ responseBody response


-- | Get the most recent 25 comments on a subreddit. This endpoint is
-- undocumented!
subComments :: Text -> ReaderT Env IO ByteString
subComments sr = do
  env <- ask
  checkTokenValidity
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "r" /: sr /: "comments"
    response <- req GET uri NoReqBody bsResponse (withUAToken env)
    pure $ responseBody response


-- TODO: Model types more properly
