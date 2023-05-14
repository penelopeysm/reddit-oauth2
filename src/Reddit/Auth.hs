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

--
-- Scopes are used to limit the things which a token can do. You should ensure
-- that you only request a token for the scopes that you need. When
-- authenticating with Reddit, the user will be able to see which scopes are
-- being requested.
--
-- To see which scope(s) are required by each endpoint, see
-- https://www.reddit.com/dev/api/oauth. Note that this library implements only
-- a small subset of endpoints, so most of these are not very useful right now.

-- | All possible OAuth2 scopes. You can obtain this list via
-- https://reddit.com/api/v1/scopes.
data Scope
  = -- | Spend the logged in user's Reddit gold.
    ScopeCreddits
  | -- | Access mod notes for subreddits moderated by the logged in user.
    ScopeModNote
  | -- | Add/remove users to/from approved user lists, ban/unban, and mute/unmute users on subreddits moderated by the logged in user.
    ScopeModContributors
  | -- | Access and manage modmail for subreddits moderated by the logged in user.
    ScopeModMail
  | -- | Manage the configuration, sidebar, and CSS for subreddits moderated by the logged in user.
    ScopeModConfig
  | -- | Manage subreddit subscriptions as well as \'friends\'.
    ScopeSubscribe
  | -- | Edit structured styles for subreddits moderated by the logged in user.
    ScopeStructuredStyles
  | -- | Vote on posts and comments.
    ScopeVote
  | -- | Edit wiki pages.
    ScopeWikiEdit
  | -- | Access the list of subreddits moderated, contributed to, and subscribed to.
    ScopeMySubreddits
  | -- | Submit links and comments.
    ScopeSubmit
  | -- | Access the moderation log in subreddits moderated by the logged in user.
    ScopeModLog
  | -- | Approve, remove, mark as (N)SFW, and distinguish content in subreddits moderated by the logged in user.
    ScopeModPosts
  | -- | Manage and assign flair in subreddits moderated by the logged in user.
    ScopeModFlair
  | -- | Save and unsave posts and comments.
    ScopeSave
  | -- | Invite and remove moderators on subreddits moderated by the logged in user.
    ScopeModOthers
  | -- | Access posts and comments.
    ScopeRead
  | -- | Read and send private messages. (Note: this does not include chat. There is, at present, no way to access chat via the public Reddit API.)
    ScopePrivateMessages
  | -- | Report, hide, and unhide content.
    ScopeReport
  | -- | Get Reddit username and signup date.
    ScopeIdentity
  | -- | Manage settings and contributors of live threads.
    ScopeLiveManage
  | -- | Update preferences and other account information. Does not grant access to email or password.
    ScopeAccount
  | -- | Access traffic stats in subreddits moderated by the user.
    ScopeModTraffic
  | -- | Read wiki pages.
    ScopeWikiRead
  | -- | Edit and delete posts and comments.
    ScopeEdit
  | -- | Change editors and visibility of wiki pages in subreddits moderated by the logged in user.
    ScopeModWiki
  | -- | Accept invitations to moderate a subreddit and step down from moderating subreddits.
    ScopeModSelf
  | -- | Access voting history, saved submissions, and hidden submissions.
    ScopeHistory
  | -- | Edit subreddit and post flair.
    ScopeFlair
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | A quick way to get all scopes.
allScopes :: S.Set Scope
allScopes = S.fromList [minBound .. maxBound]

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
  { tknintToken :: Text,
    tknintType :: Text,
    tknintExpiresIn :: Int,
    tknintScopes :: Text
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
parseToken ti = do
  currentTime <- getCurrentTime
  let seconds = secondsToNominalDiffTime . realToFrac $ tknintExpiresIn ti
  -- Insert 10 second buffer time just in case there's lots of time between
  -- tokenInternal and currentTime being obtained.
  let expiresAt = addUTCTime (seconds - 10) currentTime
  pure $
    Token
      { token = TE.encodeUtf8 (tknintToken ti),
        tokenType = TE.encodeUtf8 (tknintType ti),
        tokenScopes = parseScopes (tknintScopes ti),
        tokenExpiresAt = expiresAt
      }

getToken :: Credentials -> ByteString -> IO Token
getToken creds ua = getTokenInternal creds ua >>= parseToken

-- * Code flow ('authorisation code grant')

data Duration = Temporary | Permanent deriving (Eq, Ord, Show)

data AuthSettings = AuthSettings
  { authsClientID :: Text,
    authsState :: Text,
    authsRedirectUri :: Text,
    authsDuration :: Duration,
    authsScopes :: S.Set Scope
  }

mkRedditAuthURL :: AuthSettings -> Text
mkRedditAuthURL settings =
  let (maybeUri :: Maybe URI.URI) = do
        baseUri <- URI.mkURI "https://www.reddit.com/api/v1/authorize"
        clientIDKey <- URI.mkQueryKey "client_id"
        clientIDVal <- URI.mkQueryValue (authsClientID settings)
        typeKey <- URI.mkQueryKey "response_type"
        typeVal <- URI.mkQueryValue "code"
        stateKey <- URI.mkQueryKey "state"
        stateVal <- URI.mkQueryValue (authsState settings)
        redirectKey <- URI.mkQueryKey "redirect_uri"
        redirectVal <- URI.mkQueryValue (authsRedirectUri settings)
        durationKey <- URI.mkQueryKey "duration"
        durationVal <- URI.mkQueryValue (T.toLower . T.pack . show $ authsDuration settings)
        scopeKey <- URI.mkQueryKey "scope"
        scopeVal <- URI.mkQueryValue (showScopes $ authsScopes settings)
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
