-- |
-- Module      : Reddit.Auth
-- Description : Functions for authentication and token parsing.
-- Copyright   : (c) Penelope Yong 2023
-- License     : MIT
-- Maintainer  : penelopeysm@gmail.com
-- Stability   : experimental
module Reddit.Auth (Credentials (..), Token (..), getToken) where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Network.HTTP.Req
import Reddit.Types

-- | A record containing the credentials needed to authenticate with the OAuth2
-- API.
data Credentials = Credentials
  { username :: Text,
    password :: Text,
    clientID :: Text,
    clientSecret :: Text
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
                =: creds.username
              <> "password"
                =: creds.password
          )
  let req_params =
        header "user-agent" ua
          <> basicAuth (TE.encodeUtf8 (clientID creds)) (TE.encodeUtf8 (clientSecret creds))
  response <- req POST uri body lbsResponse req_params
  throwDecode (responseBody response)

-- | A parsed value of a Reddit access token.
data Token = Token
  { token :: ByteString,
    token_type :: ByteString,
    expires_at :: UTCTime,
    scopes :: S.Set Scope
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
        token_type = TE.encodeUtf8 tokenInternal._token_type,
        scopes = parseScopes tokenInternal._scope,
        expires_at = expires_at
      }

getToken :: Credentials -> ByteString -> IO Token
getToken creds ua = getTokenInternal creds ua >>= parseToken
