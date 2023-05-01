module Reddit.Auth (Credentials (..), Token (..), getToken) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Network.HTTP.Req

-- | A record containing the credentials needed to authenticate with the OAuth2
-- API.
data Credentials = Credentials
  { username :: Text,
    password :: Text,
    clientID :: Text,
    clientSecret :: Text
  }
  deriving (Show)

-- * Access tokens

-- | The raw JSON representation of an access token, as returned by Reddit.
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

-- | Obtain a raw token using credentials.
getTokenInternal :: Credentials -> IO TokenInternal
getTokenInternal creds = runReq defaultHttpConfig $ do
  let uri = https "www.reddit.com" /: "api" /: "v1" /: "access_token"
  let body =
        ReqBodyUrlEnc
          ( "grant_type"
              =: ("password" :: Text)
              <> "username"
                =: creds.username
              <> "password"
                =: creds.password
          )
  let options = basicAuth (TE.encodeUtf8 (clientID creds)) (TE.encodeUtf8 (clientSecret creds))
  response <- req POST uri body lbsResponse options
  throwDecode (responseBody response)

-- | A parsed value of a Reddit access token.
data Token = Token
  { token :: ByteString,
    token_type :: ByteString,
    expires_at :: UTCTime,
    scope :: ByteString
  }
  deriving (Show)

parseToken :: TokenInternal -> IO Token
parseToken tokenInternal = do
  currentTime <- getCurrentTime
  let seconds = secondsToNominalDiffTime . realToFrac $ tokenInternal._expires_in
  let expires_at = addUTCTime seconds currentTime
  pure $
    Token
      { token = TE.encodeUtf8 tokenInternal._access_token,
        token_type = TE.encodeUtf8 tokenInternal._token_type,
        scope = TE.encodeUtf8 tokenInternal._scope,
        expires_at = expires_at
      }

getToken :: Credentials -> IO Token
getToken creds = getTokenInternal creds >>= parseToken
