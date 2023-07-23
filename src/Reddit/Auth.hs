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
    AuthUrlParams (..),
    mkRedditAuthURL,
    Scope (..),
    parseScopes,
    showScopes,
    allScopes,
    Duration (..),
  )
where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Network.HTTP.Req
import qualified Text.URI as URI

-- | The @Credentials@ type contains all the information you need to
-- authenticate with the Reddit API.
data Credentials
  = -- | __'Resource owner' flow__
    --
    -- Formally, this is the 'Resource Owner Password Credentials Grant' for
    -- OAuth 2.0, which is described in [Section 4.3 of IETF RFC
    -- 6749](https://datatracker.ietf.org/doc/html/rfc6749#section-4.3).
    --
    -- To authenticate in this manner, you will need a username and password for
    -- a Reddit account, plus a client ID and client secret associated with a
    -- Reddit app. The username in question must be listed as a developer of the
    -- app. You can set up a new app by:
    --
    -- 1. Log in using the account you want to post or query Reddit as (this
    -- may be a bot account, for example).
    --
    -- 2. Navigate to <https://www.reddit.com/prefs/apps>
    --
    -- 3. Click \'create another app\' at the bottom and select the \'script\'
    -- radio button
    --
    -- 4. Fill in the required details. For a personal script, the redirect URI
    -- is not important, set it to anything you like.
    --
    -- 5. Take note of the app ID (a string with random characters) and the
    -- secret (the same but longer). These become, respectively,  @clientID@ and
    -- @clientSecret@.
    --
    -- 6. @username@ and @password@ are the Reddit login credentials of the
    -- account you are using.
    --
    -- /Note:/ __Do NOT ever share your password or the client secret publicly!__
    --
    -- See also:
    -- <https://github.com/reddit-archive/reddit/wiki/OAuth2-Quick-Start-Example>
    OwnerCredentials
      { ownerUsername :: Text,
        ownerPassword :: Text,
        ownerClientId :: Text,
        ownerClientSecret :: Text
      }
  | -- | __'Code grant' flow__
    --
    -- Formally, this is the 'Authorisation Code Grant' for OAuth 2.0, which is
    -- described in [Section 4.1 of IETF RFC
    -- 6749](https://datatracker.ietf.org/doc/html/rfc6749#section-4.1).
    --
    -- Begin by [setting up a new Reddit app](https://www.reddit.com/prefs/apps)
    -- in the same way as for the 'resource owner' flow above. Then:
    --
    -- 1. 'codeGrantClientId' and 'codeGrantClientSecret' are simply the 'app ID'
    -- and 'app secret' from the Reddit app.
    --
    -- 2. 'codeGrantRedirectUri' is the redirect URI you specified when setting
    -- up the app. Note that this must match exactly.
    --
    -- 3. To obtain 'codeGrantCode', you must first construct the Reddit
    -- authentication link using 'mkRedditAuthURL'. Send your user to this link,
    -- and they will be greeted with a Reddit page asking them to authorise your
    -- app. When they click \'allow\', Reddit will send the user back to the
    -- redirect URI which you specified, along with a \'code\' query parameter
    -- in the URI. You should then parse this and set it as 'codeGrantCode'.
    --
    -- See also:
    -- <https://github.com/reddit-archive/reddit/wiki/OAuth2>
    CodeGrantCredentials
      { codeGrantClientId :: Text,
        codeGrantClientSecret :: Text,
        codeGrantRedirectUri :: Text,
        codeGrantCode :: Text
      }
  | -- | Do not provide credentials. This is used when \'reconstructing\' a
    -- 'Reddit.RedditEnv' from an existing token (e.g. one stored in a database)
    -- using 'Reddit.mkEnvFromToken'. __You should not need to use this.__
    NoCredentials

-- * OAuth2 scopes

-- | Scopes are used to limit the things which a token can do. You should ensure
-- that you only request a token for the scopes that you need. When
-- authenticating with Reddit, the user will be able to see which scopes are
-- being requested.
--
-- To see which scope(s) are required by each endpoint, see
-- <https://www.reddit.com/dev/api/oauth>. Note that this library implements
-- only a small subset of endpoints, so most of these are not very useful right
-- now.
--
-- You can obtain this list of scopes via <https://reddit.com/api/v1/scopes>.
data Scope
  = -- | Spend the logged in user's Reddit gold.
    ScopeCreddits
  | -- | Access mod notes for subreddits moderated by the logged in user.
    ScopeModNote
  | -- | Add\/remove users to\/from approved user lists, ban\/unban, and mute\/unmute users on subreddits moderated by the logged in user.
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

-- | Parse a canonical text list of scopes, which is part of the JSON returned
-- by the Reddit authorisation server, into a set of known @Scope@s.
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

-- | Construct a canonical text representation of a set of @Scope@s, which can
-- be used as a query parameter for 'mkRedditAuthURL'.
showScopes :: S.Set Scope -> Text
showScopes = T.intercalate " " . map showOneScope . S.toList
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
    tknintScopes :: Text,
    tknintRefreshToken :: Maybe Text
  }

instance FromJSON TokenInternal where
  parseJSON = withObject "TokenInternal" $ \o -> do
    tknintToken <- o .: "access_token"
    tknintType <- o .: "token_type"
    tknintExpiresIn <- o .: "expires_in"
    tknintScopes <- o .: "scope"
    tknintRefreshToken <- o .:? "refresh_token"
    pure $ TokenInternal {..}

-- | Obtain a raw token using credentials.
getTokenInternal :: Credentials -> ByteString -> IO TokenInternal
getTokenInternal creds ua =
  -- Note that {..} RecordWildCards lets us use the field names as the values
  -- themselves, instead of as accessor functions
  case creds of
    OwnerCredentials {..} -> runReq defaultHttpConfig $ do
      let uri = https "www.reddit.com" /: "api" /: "v1" /: "access_token"
      let body =
            ReqBodyUrlEnc
              ( "grant_type"
                  =: ("password" :: Text)
                  <> "username"
                    =: ownerUsername
                  <> "password"
                    =: ownerPassword
              )
      let req_params =
            header "user-agent" ua
              <> basicAuth (TE.encodeUtf8 ownerClientId) (TE.encodeUtf8 ownerClientSecret)
      response <- req POST uri body lbsResponse req_params
      throwDecode (responseBody response)
    CodeGrantCredentials {..} -> runReq defaultHttpConfig $ do
      let uri = https "www.reddit.com" /: "api" /: "v1" /: "access_token"
      let body =
            ReqBodyUrlEnc
              ( "grant_type"
                  =: ("authorization_code" :: Text)
                  <> "redirect_uri"
                    =: codeGrantRedirectUri
                  <> "code"
                    =: codeGrantCode
              )
      let req_params =
            header "user-agent" ua
              <> basicAuth (TE.encodeUtf8 codeGrantClientId) (TE.encodeUtf8 codeGrantClientSecret)
      response <- req POST uri body lbsResponse req_params
      throwDecode (responseBody response)

-- | A parsed value of a Reddit access token.
data Token = Token
  { token :: ByteString,
    tokenType :: ByteString,
    tokenExpiresAt :: UTCTime,
    tokenScopes :: S.Set Scope,
    -- | Doesn't always exist
    tokenRefreshToken :: Maybe Text
  }

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
        tokenExpiresAt = expiresAt,
        tokenRefreshToken = tknintRefreshToken ti
      }

-- | Get a token using credentials. This is the core of 'Reddit.authenticate'.
getToken :: Credentials -> ByteString -> IO Token
getToken creds ua = getTokenInternal creds ua >>= parseToken

-- * Code flow ('authorisation code grant')

-- | This should be chosen depending on whether your app needs a temporary token
-- (i.e. user must authenticate again after the token expires), or a permanent
-- one (i.e. user does not need to authenticate again).
--
-- Internally, permanent tokens are handled by requesting a refresh token when
-- the existing one expires.
data Duration = Temporary | Permanent deriving (Eq, Ord, Show)

-- | The information required for your app to construct a link for the user to
-- 'authorise with Reddit'.
--
-- This is only required for the 'code grant' flow.
data AuthUrlParams = AuthUrlParams
  { -- | This is the client ID for your app.
    authUrlClientID :: Text,
    -- | This can be any text value you want, and must be different for every
    -- user you are authenticating. If the user authorises your app, they will
    -- be sent back to your redirect URI, and the same state will be included as
    -- a query parameter. You should check that the returned state matches the
    -- one which you specified here.
    --
    -- In principle, this is not mandatory. However, it is really bad practice
    -- to not use it, as it makes you vulnerable to CSRF attacks. See, e.g.,
    -- [Section 10.12 of the OAuth2
    -- RFC](https://datatracker.ietf.org/doc/html/rfc6749#section-10.12). So,
    -- this library forces you to specify it.
    authUrlState :: Text,
    -- | The URI which users will be sent back to after logging in. This must
    -- match the redirect URI set up in <https://reddit.com/prefs/apps>.
    authUrlRedirectUri :: Text,
    -- | 'Temporary' or 'Permanent', depending on how long you need your user to
    -- be logged in for. Temporary tokens last for 1 hour, after which the user
    -- must re-authenticate if they want to continue using your app.
    authUrlDuration :: Duration,
    -- | The OAuth2 scopes that you need. This depends on what your app does on
    -- behalf of its users. See 'Scope' for the full list.
    authUrlScopes :: S.Set Scope
  }

-- | Construct the Reddit authorisation URL that you should send your users to
-- for the 'code grant' flow.
mkRedditAuthURL ::
  Bool ->
  -- | Whether to use the mobile version of the authorisation page
  AuthUrlParams ->
  -- | The authorisation URL
  Text
mkRedditAuthURL mobile params =
  let (maybeUri :: Maybe URI.URI) = do
        baseUri <-
          if mobile
            then URI.mkURI "https://www.reddit.com/api/v1/authorize.compact"
            else URI.mkURI "https://www.reddit.com/api/v1/authorize"
        clientIDKey <- URI.mkQueryKey "client_id"
        clientIDVal <- URI.mkQueryValue (authUrlClientID params)
        typeKey <- URI.mkQueryKey "response_type"
        typeVal <- URI.mkQueryValue "code"
        redirectKey <- URI.mkQueryKey "redirect_uri"
        redirectVal <- URI.mkQueryValue (authUrlRedirectUri params)
        durationKey <- URI.mkQueryKey "duration"
        durationVal <- URI.mkQueryValue (T.toLower . T.pack . show $ authUrlDuration params)
        scopeKey <- URI.mkQueryKey "scope"
        scopeVal <- URI.mkQueryValue (showScopes $ authUrlScopes params)
        stateKey <- URI.mkQueryKey "state"
        stateVal <- URI.mkQueryValue (authUrlState params)
        let params =
              [ URI.QueryParam clientIDKey clientIDVal,
                URI.QueryParam typeKey typeVal,
                URI.QueryParam redirectKey redirectVal,
                URI.QueryParam durationKey durationVal,
                URI.QueryParam scopeKey scopeVal,
                URI.QueryParam stateKey stateVal
              ]
        pure $ baseUri {URI.uriQuery = params}
   in case maybeUri of
        Nothing -> error "mkRedditAuthURL failed"
        Just u -> URI.render u
