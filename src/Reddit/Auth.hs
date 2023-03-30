module Reddit.Auth where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Network.HTTP.Req

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
