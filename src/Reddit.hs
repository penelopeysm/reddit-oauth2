{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Reddit where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Network.HTTP.Req

data Settings = Settings
  { username :: Text,
    password :: Text,
    clientID :: Text,
    clientSecret :: Text,
    userAgent :: Text
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
  parseJSON (Object v) = TokenInternal
    <$> v .: "access_token"
    <*> v .: "token_type"
    <*> v .: "expires_in"
    <*> v .: "scope"

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
  pure $ Token {
    token = TE.encodeUtf8 (_access_token t),
    token_type = TE.encodeUtf8 (_token_type t),
    scope = TE.encodeUtf8 (_scope t),
    expires_at = expires_at
  }

authWithCredentials :: Settings -> IO Token
authWithCredentials settings = do
  tokenInternal <- runReq defaultHttpConfig $ do
    let uri = https "www.reddit.com" /: "api" /: "v1" /: "access_token"
    let body = ReqBodyUrlEnc (
              "grant_type" =: ("password" :: Text) <>
              "username" =: username settings <>
              "password" =: password settings)
    let options = basicAuth (TE.encodeUtf8 (clientID settings)) (TE.encodeUtf8 (clientSecret settings))
    response <- req POST uri body lbsResponse options
    throwDecode (responseBody response)
  convertToken tokenInternal

user :: Text -> Text -> Token -> IO ByteString
user userAgent username tkn = runReq defaultHttpConfig $ do
  let uri = https "oauth.reddit.com" /: "user" /: username /: "about"
  let options = header "user-agent" (TE.encodeUtf8 userAgent) <>
        oAuth2Bearer (token tkn)
  response <- req GET uri NoReqBody bsResponse options
  pure $ responseBody response
