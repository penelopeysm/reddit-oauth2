-- |
-- Module      : Reddit.Auth
-- Description : Functions for authentication and token parsing.
-- Copyright   : (c) Penelope Yong 2023
-- License     : MIT
-- Maintainer  : penelopeysm@gmail.com
-- Stability   : experimental
module Reddit.Auth
  ( Credentials (..),
    Token (..),
    getToken,
    AuthSettings (..),
    mkRedditAuthURL,
    Scope (..),
    allScopes,
    Duration (..),
  )
where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Network.HTTP.Req
import Reddit.Types
import qualified Text.URI as URI

-- | A record containing the credentials needed to authenticate with the OAuth2
-- API.
data Credentials = Credentials
  { credsUsername :: Text,
    credsPassword :: Text,
    credsClientId :: Text,
    credsClientSecret :: Text
  }

-- * OAuth2 scopes

data Scope
  = ScopeCreddits
  | ScopeModNote
  | ScopeModContributors
  | ScopeModMail
  | ScopeModConfig
  | ScopeSubscribe
  | ScopeStructuredStyles
  | ScopeVote
  | ScopeWikiEdit
  | ScopeMySubreddits
  | ScopeSubmit
  | ScopeModLog
  | ScopeModPosts
  | ScopeModFlair
  | ScopeSave
  | ScopeModOthers
  | ScopeRead
  | ScopePrivateMessages
  | ScopeReport
  | ScopeIdentity
  | ScopeLiveManage
  | ScopeAccount
  | ScopeModTraffic
  | ScopeWikiRead
  | ScopeEdit
  | ScopeModWiki
  | ScopeModSelf
  | ScopeHistory
  | ScopeFlair
  deriving (Eq, Ord, Enum, Bounded, Show)

parseScopes :: Text -> S.Set Scope
parseScopes t = case T.split (\c -> c == ',' || c == ' ') t of
  [] -> S.empty
  [x] -> case x of
    "" -> S.empty
    "*" -> S.fromList [minBound .. maxBound] -- All scopes
    "account" -> S.singleton ScopeAccount
    "creddits" -> S.singleton ScopeCreddits
    "edit" -> S.singleton ScopeEdit
    "flair" -> S.singleton ScopeFlair
    "history" -> S.singleton ScopeHistory
    "identity" -> S.singleton ScopeIdentity
    "livemanage" -> S.singleton ScopeLiveManage
    "modconfig" -> S.singleton ScopeModConfig
    "modcontributors" -> S.singleton ScopeModContributors
    "modflair" -> S.singleton ScopeModFlair
    "modlog" -> S.singleton ScopeModLog
    "modmail" -> S.singleton ScopeModMail
    "modnote" -> S.singleton ScopeModNote
    "modothers" -> S.singleton ScopeModOthers
    "modposts" -> S.singleton ScopeModPosts
    "modself" -> S.singleton ScopeModSelf
    "modtraffic" -> S.singleton ScopeModTraffic
    "modwiki" -> S.singleton ScopeModWiki
    "mysubreddits" -> S.singleton ScopeMySubreddits
    "privatemessages" -> S.singleton ScopePrivateMessages
    "read" -> S.singleton ScopeRead
    "report" -> S.singleton ScopeReport
    "save" -> S.singleton ScopeSave
    "structuredstyles" -> S.singleton ScopeStructuredStyles
    "submit" -> S.singleton ScopeSubmit
    "subscribe" -> S.singleton ScopeSubscribe
    "vote" -> S.singleton ScopeVote
    "wikiedit" -> S.singleton ScopeWikiEdit
    "wikiread" -> S.singleton ScopeWikiRead
  xs -> mconcat (map parseScopes xs)

showScopes :: S.Set Scope -> Text
showScopes = T.intercalate "," . map showOneScope . S.toList
  where
    showOneScope s = case s of
      ScopeAccount -> "account"
      ScopeCreddits -> "creddits"
      ScopeEdit -> "edit"
      ScopeFlair -> "flair"
      ScopeHistory -> "history"
      ScopeIdentity -> "identity"
      ScopeLiveManage -> "livemanage"
      ScopeModConfig -> "modconfig"
      ScopeModContributors -> "modcontributors"
      ScopeModFlair -> "modflair"
      ScopeModLog -> "modlog"
      ScopeModMail -> "modmail"
      ScopeModNote -> "modnote"
      ScopeModOthers -> "modothers"
      ScopeModPosts -> "modposts"
      ScopeModSelf -> "modself"
      ScopeModTraffic -> "modtraffic"
      ScopeModWiki -> "modwiki"
      ScopeMySubreddits -> "mysubreddits"
      ScopePrivateMessages -> "privatemessages"
      ScopeRead -> "read"
      ScopeReport -> "report"
      ScopeSave -> "save"
      ScopeStructuredStyles -> "structuredstyles"
      ScopeSubmit -> "submit"
      ScopeSubscribe -> "subscribe"
      ScopeVote -> "vote"
      ScopeWikiEdit -> "wikiedit"
      ScopeWikiRead -> "wikiread"

allScopes :: S.Set Scope
allScopes = S.fromList [minBound .. maxBound]

-- * Access tokens

-- | The raw JSON representation of an access token, as returned by Reddit.
data TokenInternal = TokenInternal
  { _access_token :: Text,
    _token_type :: Text,
    _expires_in :: Int,
    _scope :: Text
  }

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
getTokenInternal :: Credentials -> ByteString -> IO TokenInternal
getTokenInternal creds ua = runReq defaultHttpConfig $ do
  let uri = https "www.reddit.com" /: "api" /: "v1" /: "access_token"
  let body =
        ReqBodyUrlEnc
          ( "grant_type"
              =: ("password" :: Text)
              <> "username"
                =: credsUsername creds
              <> "password"
                =: credsPassword creds
          )
  let req_params =
        header "user-agent" ua
          <> basicAuth (TE.encodeUtf8 (credsClientId creds)) (TE.encodeUtf8 (credsClientSecret creds))
  response <- req POST uri body lbsResponse req_params
  throwDecode (responseBody response)

-- | A parsed value of a Reddit access token.
data Token = Token
  { token :: ByteString,
    tokenType :: ByteString,
    tokenExpiresAt :: UTCTime,
    tokenScopes :: S.Set Scope
  }
  deriving (Show)

parseToken :: TokenInternal -> IO Token
parseToken tokenInternal = do
  currentTime <- getCurrentTime
  let seconds = secondsToNominalDiffTime . realToFrac $ tokenInternal._expires_in
  -- Insert 10 second buffer time just in case there's lots of time between
  -- tokenInternal and currentTime being obtained.
  let expires_at = addUTCTime (seconds - 10) currentTime
  pure $
    Token
      { token = TE.encodeUtf8 tokenInternal._access_token,
        tokenType = TE.encodeUtf8 tokenInternal._token_type,
        tokenScopes = parseScopes tokenInternal._scope,
        tokenExpiresAt = expires_at
      }

getToken :: Credentials -> ByteString -> IO Token
getToken creds ua = getTokenInternal creds ua >>= parseToken

-- * Code flow ('authorisation code grant')

data Duration = Temporary | Permanent deriving (Eq, Ord, Show)

data AuthSettings = AuthSettings
  { clientID :: Text,
    state :: Text,
    redirectUri :: Text,
    duration :: Duration,
    scopes :: S.Set Scope
  }

mkRedditAuthURL :: AuthSettings -> Text
mkRedditAuthURL settings =
  let (maybeUri :: Maybe URI.URI) = do
        baseUri <- URI.mkURI "https://www.reddit.com/api/v1/authorize"
        clientIDKey <- URI.mkQueryKey "client_id"
        clientIDVal <- URI.mkQueryValue settings.clientID
        typeKey <- URI.mkQueryKey "response_type"
        typeVal <- URI.mkQueryValue "code"
        stateKey <- URI.mkQueryKey "state"
        stateVal <- URI.mkQueryValue settings.state
        redirectKey <- URI.mkQueryKey "redirect_uri"
        redirectVal <- URI.mkQueryValue settings.redirectUri
        durationKey <- URI.mkQueryKey "duration"
        durationVal <- URI.mkQueryValue (T.toLower . T.pack . show $ settings.duration)
        scopeKey <- URI.mkQueryKey "scope"
        scopeVal <- URI.mkQueryValue (showScopes settings.scopes)
        let params =
              [ URI.QueryParam clientIDKey clientIDVal,
                URI.QueryParam typeKey typeVal,
                URI.QueryParam stateKey stateVal,
                URI.QueryParam redirectKey redirectVal,
                URI.QueryParam durationKey durationVal,
                URI.QueryParam scopeKey scopeVal
              ]
        pure $ baseUri {URI.uriQuery = params}
   in case maybeUri of
        Nothing -> error $ T.unpack "mkRedditAuthURL failed"
        Just u -> URI.render u
