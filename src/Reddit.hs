{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Reddit
-- Description : Functions to interact with the Reddit OAuth2 API
-- Copyright   : (c) Penelope Yong 2023
-- License     : MIT
-- Maintainer  : penelopeysm@gmail.com
-- Stability   : experimental
--
-- @reddit-oauth2@ provides a collection of functions to query the Reddit OAuth2 API.
--
-- It is currently very primitive, but contains enough stuff to let you make a
-- bot that replies to certain phrases found in comments / posts (which is a
-- very common use case). If you are interested in this specific use case, the
-- fastest way to get up to scratch is probably to see the example in the
-- "Reddit.Example" module.
--
-- Otherwise, the remainder of this page should be fairly well-documented and
-- otherwise self-explanatory. Please feel free to raise an issue if there are
-- any unclear aspects.
module Reddit
  ( -- * The @RedditT@ monad
    RedditT (..),
    runRedditT,
    runRedditT',
    runRedditTCleanup,
    runRedditTCleanup',
    RedditEnv,
    -- | #credentials#

    -- * Authentication with account credentials
    -- $credentials
    Auth.Credentials (..),
    authenticate,
    revokeToken,

    -- * Authentication with code flow
    -- $code-flow
    Auth.AuthSettings (..),
    Auth.mkRedditAuthURL,
    Auth.Scope (..),
    Auth.allScopes,
    Auth.Duration (..),

    -- * Comments

    --
    -- $comments
    Reddit.Types.Comment (..),
    Reddit.Types.CommentTree (..),
    getComments,
    getComment,
    addNewComment,
    accountComments,
    subredditComments,
    getPostWithComments,
    expandTree,
    expandTreeFully,

    -- * Accounts (i.e. users)

    --
    -- $accounts
    Reddit.Types.Account (..),
    getAccounts,
    getAccount,
    getAccountByName,

    -- * Posts

    --
    -- $posts
    Reddit.Types.Post (..),
    getPosts,
    getPost,
    accountPosts,
    Timeframe (..),
    SubredditSort (..),
    subredditPosts,
    edit,
    delete,
    remove,
    approve,

    -- * Messages

    --
    -- $messages
    Reddit.Types.Message (..),

    -- * Subreddits

    --
    -- $subreddits
    Reddit.Types.Subreddit (..),
    getSubreddits,
    getSubreddit,
    getSubredditsByName,
    getSubredditByName,

    -- * Awards

    --
    -- $awards
    Reddit.Types.Award (..),
    -- | #streams#

    -- * Streams
    -- $streams
    StreamSettings (..),
    defaultStreamSettings,
    stream,
    stream',
    postStream,
    commentStream,
    -- | #listings#

    -- * Listings
    -- $listings

    -- * Other types
    Reddit.Types.ID (..),
    Reddit.Types.HasID (..),
    Reddit.Types.CanCommentOn (..),
    Reddit.Types.EditedUTCTime (..),
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (..), finally, throwIO)
import Control.Monad (foldM)
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (Value (..), parse)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.IORef as R
import Data.List (union, (\\))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Time.Clock
import GHC.Float.RealFracMethods (floorDoubleInt)
import Network.HTTP.Req
import qualified Reddit.Auth as Auth
import qualified Reddit.Queue as Q
import Reddit.Types

-- | Everything required to query the Reddit API. In principle, you should not
-- need to edit anything here, so the field accessors are not exported.
data RedditEnv = RedditEnv
  { -- | OAuth2 token contents.
    envTokenRef :: R.IORef Auth.Token,
    -- | Credentials used to log in. Must be stored to allow for
    -- reauthentication if and when the token expires.
    envUsername :: Text,
    envPassword :: HiddenText,
    envClientId :: Text,
    envClientSecret :: HiddenText,
    -- | User-agent. Should be unique.
    envUserAgent :: ByteString
  }

-- | The type of a Reddit computation.
type RedditT = ReaderT RedditEnv IO

-- | Run a Reddit computation using a @RedditEnv@ value obtained through
-- authentication (see [Authentication](#credentials)).
runRedditT :: RedditT a -> RedditEnv -> IO a
runRedditT = runReaderT

-- | @runRedditT'@ is @flip 'runRedditT'@ and is slightly more ergonomic.
runRedditT' :: RedditEnv -> RedditT a -> IO a
runRedditT' = flip runReaderT

-- | Run a Reddit computation, and additionally clean up the @RedditEnv@ value
-- by revoking the active access token after the computation finishes.
--
-- Doing this is generally good behaviour. Alternatively, you can manually use
-- 'revokeToken'.
runRedditTCleanup :: RedditT a -> RedditEnv -> IO a
runRedditTCleanup actn env =
  finally (runRedditT actn env) (revokeToken env)

-- | Same as @flip 'runRedditTCleanup'@.
runRedditTCleanup' :: RedditEnv -> RedditT a -> IO a
runRedditTCleanup' = flip runRedditTCleanup

-- $credentials
--
-- This section describes the credentials needed for using the Reddit API as a
-- \'script\'. Specifically, if you are developing an application exclusively
-- for personal use, then this is suitable. Most Reddit bots fall under this
-- case.
--
-- Formally, this is the 'Resource Owner Password Credentials Grant' for OAuth
-- 2.0, which is described in [Section 4.3 of IETF RFC
-- 6749](https://datatracker.ietf.org/doc/html/rfc6749#section-4.3).
--
-- To authenticate in this manner, you will need a username and password for a
-- Reddit account, plus a client ID and client secret associated with a Reddit
-- app. The username in question must be listed as a developer of the app. You
-- can set up a new app by:
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

-- | Authenticate using credentials, which gives you a token contained inside a
-- 'RedditEnv'. This @RedditEnv@ value is required to perform all Reddit
-- queries.
authenticate ::
  Auth.Credentials ->
  -- | Your user-agent. [Reddit
  -- says](https://github.com/reddit-archive/reddit/wiki/API) you should use a
  -- unique and identifiable user-agent.
  Text ->
  IO RedditEnv
authenticate creds ua = do
  let uaBS = TE.encodeUtf8 ua
  tokenRef <- Auth.getToken creds uaBS >>= R.newIORef
  pure $
    RedditEnv
      { envTokenRef = tokenRef,
        envUsername = Auth.credsUsername creds,
        envPassword = HiddenText (Auth.credsPassword creds),
        envClientId = Auth.credsClientId creds,
        envClientSecret = HiddenText (Auth.credsClientSecret creds),
        envUserAgent = uaBS
      }

-- | This function is not exported, but is used when the access token has to be
-- updated (i.e. when it expires). It updates the contents of the IORefs in the
-- RedditEnv with an updated token.
reauthenticate :: RedditEnv -> IO ()
reauthenticate env = do
  let creds =
        Auth.Credentials
          { credsUsername = envUsername env,
            credsPassword = getHiddenText (envPassword env),
            credsClientId = envClientId env,
            credsClientSecret = getHiddenText (envClientSecret env)
          }
  token <- Auth.getToken creds (envUserAgent env)
  R.writeIORef (envTokenRef env) token

-- | Perform a RedditT action, but before running it, check the validity of the
-- existing token and reauthenticate if it has expired already.
--
-- Generally, we only need to use this in functions which are actually querying
-- a Reddit API endpoint (it doesn't need to be indiscriminately applied to all
-- functions with a RedditT type).
withTokenCheck :: RedditT a -> RedditT a
withTokenCheck action = do
  env <- ask
  t <- liftIO $ R.readIORef (envTokenRef env)
  let expiryTime = Auth.tokenExpiresAt t
  currentTime <- liftIO getCurrentTime
  when
    (nominalDiffTimeToSeconds (diffUTCTime currentTime expiryTime) > 0)
    (liftIO $ reauthenticate env)
  action

oauth :: Text
oauth = "oauth.reddit.com"

-- | Convenience function to generate HTTP headers required for basic
-- authentication.
withUAToken :: (MonadIO m) => RedditEnv -> m (Option 'Https)
withUAToken env = do
  t <- liftIO $ R.readIORef (envTokenRef env)
  pure $ header "user-agent" (envUserAgent env) <> oAuth2Bearer (Auth.token t)

-- | Revoke an access token contained in a @RedditEnv@, rendering it unusable.
-- If you want to continue performing queries after this, you will need to
-- generate a new @RedditEnv@.
revokeToken :: RedditEnv -> IO ()
revokeToken env = do
  t <- R.readIORef (envTokenRef env)
  uat <- withUAToken env
  void $ runReq defaultHttpConfig $ do
    let uri = https "www.reddit.com" /: "api" /: "v1" /: "revoke_token"
    let body_params =
          "token" =: TE.decodeUtf8 (Auth.token t)
            <> "token_type_hint" =: TE.decodeUtf8 (Auth.tokenType t)
    req POST uri (ReqBodyUrlEnc body_params) ignoreResponse uat

-- $code-flow
--
-- Formally, this is the 'Authorisation Code Grant' for OAuth 2.0, which is
-- described in [Section 4.1 of IETF RFC
-- 6749](https://datatracker.ietf.org/doc/html/rfc6749#section-4.1).
--
-- Blah blah....

-- $listings
-- Listings are Reddit's way of paginating things (i.e. comments, posts, etc.).
-- When you ask for a series of things, the Reddit API really returns a listing
-- of things, from which the posts themselves must be extracted.
--
-- This library abstracts this away from you completely, so you should not have
-- to deal with any listings themselves. However, there's a good reason to know
-- about how listings work, namely efficiency.
--
-- When making a single HTTP request to the Reddit API, only 100 things can be
-- returned in a single listing. So, if you want to fetch the first 200 comments
-- of a user, with @'accountComments' 200 "username"@, then we need to perform
-- two HTTP requests. Of course, this library does that for you
-- automatically—but it will take longer than you might initially expect.
--
-- This goes up to a maximum of 1000, after which Reddit refuses to return any
-- more results. So, even if you ask for 1500 comments, you'll only get the
-- first 1000. Also, if you ask for 1000 comments and the user has only ever
-- commented 500 times, you'll get all 500.
--
-- This leads us to three guidelines for choosing the @Int@ parameter for
-- functions such as 'accountComments', 'accountPosts', etc.:
--
-- 1. Keep it to 100 or lower if you want it to complete within a single
--    request. In particular, you probably don't want to make this greater than
--    100 if you're creating a [stream](#streams).
-- 2. If you just want 'as many as possible', put 1000.
-- 3. Don't bother putting anything larger than 1000.

-- | Internal function to fetch n <= 100 results from a given URI. Assumes that
-- the listing will be homogeneous.
--
-- This assumes that the endpoint accepts the \'limit\' and \'after\'
-- URL-encoded parameters.
getListingSingle ::
  (HasID t, FromJSON t) =>
  -- | The number of things to ask for. Should really be 100 or fewer.
  Int ->
  -- | The value of @after@ to pass in the query.
  Maybe Text ->
  -- | The URL to query
  Url 'Https ->
  -- | URL-encoded params. Use @mempty@ if not needed.
  Option 'Https ->
  -- | The things, plus the \'after\' field returned by Reddit.
  RedditT ([t], Maybe Text)
getListingSingle size aft url in_params = withTokenCheck $ do
  env <- ask
  uat <- withUAToken env
  when (size > 100) (error "getListingSingle: size should be 100 or less. This is a bug, please report it.")
  let params =
        in_params
          <> uat
          <> "limit" =: size
          <> ( case aft of
                 Just t -> "after" =: t
                 Nothing -> mempty
             )
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    response <- req GET url NoReqBody lbsResponse params
    pure (responseBody response)
  listing <- throwDecode respBody
  pure (contents listing, after listing)

-- | Internal function to fetch n <= 1000 results from a given URI. Assumes that
-- the returned listing will be homogeneous.
--
-- This assumes that the endpoint accepts the \'count\' and \'after\'
-- URL-encoded parameters.
getListings ::
  (HasID t, FromJSON t) =>
  -- | The number of things to ask for. Must be 100 or fewer.
  Int ->
  -- | The value of \'after\'.
  Maybe Text ->
  -- | The URL to query
  Url 'Https ->
  -- | URL-encoded params. Use @mempty@ if not needed.
  Option 'Https ->
  -- | All the things, concatenated into a single list.
  RedditT [t]
getListings size aft uri in_params = do
  if size <= 100
    then fst <$> getListingSingle size aft uri in_params
    else do
      (first_batch, aft') <- getListingSingle 100 aft uri in_params
      case aft' of
        Nothing -> pure first_batch -- No more stuff to get.
        Just a -> do
          remainder <- getListings (size - 100) (Just a) uri in_params
          -- This is O(N^2) and ugly, but the bottleneck here is really the HTTP
          -- requests, so it's probably ok. Also, N is always <= 1000.
          pure (first_batch ++ remainder)

-- | This is a generic function querying the /api/info endpoint.
--
-- TODO: Fix when the list is more than 100 elements long.
getThingsByIDs :: (HasID t, FromJSON t) => [ID t] -> RedditT [t]
getThingsByIDs ids = withTokenCheck $ do
  env <- ask
  uat <- withUAToken env
  let fullNames = T.intercalate "," (map mkFullNameFromID ids)
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "api" /: "info"
    let params = uat <> "id" =: fullNames
    response <- req GET uri NoReqBody lbsResponse params
    pure (responseBody response)
  ct <- contents <$> throwDecode respBody
  when
    (length ct /= length ids)
    (fail $ "Reddit response had incorrect length: expected " <> show (length ids) <> ", found " <> show (length ct))
  pure ct

getThingByID :: (HasID t, FromJSON t) => ID t -> RedditT t
getThingByID its_id = do
  thing <- getThingsByIDs [its_id]
  case thing of
    [t] -> pure t
    _ -> fail "Reddit response had incorrect length"

-- $comments
-- Comments.

-- | Fetch a list of comments by their IDs. More efficient than @map getComment@
-- because it only makes one API call.
--
-- This function can work with any collection of comments (they may, for
-- example, be scattered amongst a whole range of posts). However, it does not
-- return a list of replies for each comment. If you want to see comment
-- replies, you need to use 'getPostWithComments', which internally uses a
-- different API endpoint.
getComments :: [ID Comment] -> RedditT [Comment]
getComments = getThingsByIDs

-- | Fetch a single comment given its ID. The same caveat described for
-- 'getComments' also applies here.
getComment :: ID Comment -> RedditT Comment
getComment = getThingByID

-- | Add a new comment as a reply to an existing post or comment.
addNewComment ::
  -- \| This constraint ensures that you can only reply to comments and posts.
  (CanCommentOn a) =>
  -- | The ID of the thing being replied to.
  ID a ->
  -- | The contents of the comment (in Markdown)
  Text ->
  RedditT ()
addNewComment x body = withTokenCheck $ do
  env <- ask
  uat <- withUAToken env
  let fullName = mkFullNameFromID x
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "api" /: "comment"
    let body_params = "thing_id" =: fullName <> "text" =: body
    void $ req POST uri (ReqBodyUrlEnc body_params) ignoreResponse uat

-- | Get the most recent comments by a user.
accountComments ::
  -- | Number of comments to fetch (maximum 1000). See [the listings
  -- section](#listings) for an explanation of this parameter.
  Int ->
  -- | Username (without the @\/u\/@).
  Text ->
  RedditT [Comment]
accountComments n uname =
  let uri = (https oauth /: "user" /: uname /: "comments")
   in getListings n Nothing uri mempty

-- | Get the most recent comments on a subreddit.
subredditComments ::
  -- | Number of comments to fetch (maximum 1000). See [the listings
  -- section](#listings) for an explanation of this parameter.
  Int ->
  -- | Subreddit name (without the @\/r\/@).
  Text ->
  RedditT [Comment]
subredditComments n sr =
  let uri = https oauth /: "r" /: sr /: "comments"
   in getListings n Nothing uri mempty

-- | Retrieve a post, together with a tree of comments on it.
--
-- This comment tree does not necessarily contain all comments on the post. For
-- posts with many comments, Reddit often hides comments which are above a
-- certain depth / very low in the thread. This can be seen on the website,
-- where you need to click 'X more replies' to load the comments fully. In this
-- library, this is modelled as a 'MoreComments' value.
--
-- The Reddit API does not provide any way to directly obtain these collapsed
-- replies: instead, they must be expanded one by one. You can use either
-- 'expandTree' to perform expansion step-by-step, or use 'expandTreeFully' to
-- recursively expand every collapsed reply.
getPostWithComments :: ID Post -> RedditT (Post, [CommentTree])
getPostWithComments (PostID p) = withTokenCheck $ do
  env <- ask
  uat <- withUAToken env
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "comments" /: p
    response <- req GET uri NoReqBody lbsResponse uat
    pure (responseBody response)
  -- Reddit returns a JSON array where the first item is a listing containing
  -- the post, and the second a listing containing the comments. Thankfully, the
  -- default implementation for FromJSON (a, b) does exactly this.
  (postListing, cmtListing) <- throwDecode respBody
  case contents postListing of
    [p] -> pure (p, contents cmtListing)
    _ -> fail "Expected one post, got many"

-- | Fetch unshown comments on a post.
getMoreChildren :: ID Post -> [ID Comment] -> RedditT [MoreChildren]
getMoreChildren pid cids =
  if null cids
    then pure []
    else withTokenCheck $ do
      env <- ask
      uat <- withUAToken env
      respBody <- liftIO $ runReq defaultHttpConfig $ do
        let uri = https oauth /: "api" /: "morechildren"
        let query_params =
              uat
                <> "api_type" =: ("json" :: Text)
                <> "link_id" =: mkFullNameFromID pid
                <> "children" =: T.intercalate "," (map unCommentID cids)
        response <- req GET uri NoReqBody lbsResponse query_params
        pure (responseBody response)
      -- The actual data we want is nested a few levels down.
      Object v <- throwDecode respBody
      case parse (\v -> v .: "json" >>= (.: "data") >>= (.: "things")) v of
        Error err -> fail err
        -- 'things' is the actual array of stuff we want
        Success (things :: Value) -> case parse parseJSON things of
          Error err' -> fail err'
          Success result -> pure result

-- | Expand the first instance of @MoreComments@ in a tree. The required post ID
-- is that of the post for which you are expanding comments.
expandTree :: ID Post -> [CommentTree] -> RedditT [CommentTree]
expandTree pid trees = case getFirstMore trees of
  Nothing -> pure trees
  Just m@(MoreComments cids) -> do
    children <- getMoreChildren pid cids
    let remaining = removeFromTrees m trees
    pure $ addChildrenToTree children remaining
  _ -> error "Shouldn't happen"

-- | Expand all instances of @MoreComments@ in a tree (including those unveiled
-- by expanding previous @MoreComments@).
--
-- Essentially, if you want to obtain every single comment on a post given its
-- ID @(pid :: ID Post)@, then you can do:
--
-- @
-- (_, trees) <- getPostWithComments pid
-- expandedTrees <- expandTreeFully pid trees
-- @
--
-- Note that this can take quite a bit of time to run for posts with thousands
-- of comments. It can't be sped up either, because [Reddit's
-- API](https://www.reddit.com/dev/api/#GET_api_morechildren) stipulates that
-- you cannot make concurrent requests to this endpoint.
expandTreeFully :: ID Post -> [CommentTree] -> RedditT [CommentTree]
expandTreeFully pid trees = case getFirstMore trees of
  Nothing -> pure trees
  Just _ -> do
    trees' <- expandTree pid trees
    let sz = sum (map treeSize trees)
    let sz' = sum (map treeSize trees')
    expandTreeFully pid trees'

-- $accounts
-- Accounts.

-- | Fetch a list of accounts given their IDs. More efficient than @map
-- getAccount@ because it only makes one API call.
getAccounts :: [ID Account] -> RedditT [Account]
getAccounts = getThingsByIDs

-- | Fetch a single account given its ID.
getAccount :: ID Account -> RedditT Account
getAccount = getThingByID

-- | Fetch details about a named user.
getAccountByName ::
  -- | Username (without the @\/u\/@).
  Text ->
  RedditT Account
getAccountByName uname = withTokenCheck $ do
  env <- ask
  uat <- withUAToken env
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "user" /: uname /: "about"
    response <- req GET uri NoReqBody lbsResponse uat
    pure (responseBody response)
  throwDecode respBody

-- | The timeframe over which to get top or most controversial posts.
data Timeframe = Hour | Day | Week | Month | Year | All

-- | Sorting order for posts in a subreddit.
data SubredditSort = Hot | New | Random | Rising | Top Timeframe | Controversial Timeframe

-- | Fetch a list of posts by their IDs. More efficient than @map getPost@
-- because it only makes one API call.
getPosts :: [ID Post] -> RedditT [Post]
getPosts = getThingsByIDs

-- | Fetch a single post given its ID.
--
-- If you want to fetch a post together with its comments, you can use
-- 'getPostWithComments'.
getPost :: ID Post -> RedditT Post
getPost = getThingByID

-- | Get the most recent posts by a user.
accountPosts ::
  -- | Number of posts to fetch (maximum 1000). See [the listings
  -- section](#listings) for an explanation of this parameter.
  Int ->
  -- | Username (without the @\/u\/@).
  Text ->
  RedditT [Post]
accountPosts n uname = do
  let uri = https oauth /: "user" /: uname /: "submitted"
   in getListings n Nothing uri mempty

-- | Get the posts from the front page of a subreddit, with the ordering
-- specified in the @SubredditSort@ argument.
subredditPosts ::
  -- | Number of posts to fetch (maximum 1000). See [the listings
  -- section](#listings) for an explanation of this parameter.
  Int ->
  -- | Subreddit name (without the @\/r\/@).
  Text ->
  -- | Sort type for the subreddit posts. Entirely analogous to the options when
  -- browsing Reddit on the web.
  SubredditSort ->
  RedditT [Post]
subredditPosts n sr sort = do
  let timeframe_text tf = case tf of
        Hour -> "hour" :: Text -- the type checker needs help
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
  let uri = https oauth /: "r" /: sr /: endpoint
   in getListings n Nothing uri tf_params

-- | Edit the contents of a comment or a text post. You must be authenticated as
-- the person who posted it.
edit :: (CanCommentOn a) => ID a -> Text -> RedditT ()
edit id newBody = withTokenCheck $ do
  let fullName = mkFullNameFromID id
  env <- ask
  uat <- withUAToken env
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "api" /: "editusertext"
    let body_params = "thing_id" =: fullName <> "text" =: newBody
    void $ req POST uri (ReqBodyUrlEnc body_params) ignoreResponse uat

-- | Delete a post or a comment. You must be authenticated as the person who
-- posted it.
--
-- If you want to remove a post or a comment on a subreddit you moderate, use
-- 'remove' instead.
delete :: (CanCommentOn a) => ID a -> RedditT ()
delete id = withTokenCheck $ do
  let fullName = mkFullNameFromID id
  env <- ask
  uat <- withUAToken env
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "api" /: "del"
    let body_params = "id" =: fullName
    void $ req POST uri (ReqBodyUrlEnc body_params) ignoreResponse uat

-- | As a moderator, remove a post or a comment. This is the inverse of
-- 'approve'.
remove ::
  (CanCommentOn a) =>
  ID a ->
  -- | Whether the thing being removed is spam.
  Bool ->
  RedditT ()
remove id isSpam = withTokenCheck $ do
  let fullName = mkFullNameFromID id
  let spam :: Text = if isSpam then "true" else "false"
  env <- ask
  uat <- withUAToken env
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "api" /: "remove"
    let body_params = "id" =: fullName <> "spam" =: spam
    void $ req POST uri (ReqBodyUrlEnc body_params) ignoreResponse uat

-- | As a moderator, approve a post or a comment. This is the inverse of
-- 'remove'.
approve :: (CanCommentOn a) => ID a -> RedditT ()
approve id = withTokenCheck $ do
  let fullName = mkFullNameFromID id
  env <- ask
  uat <- withUAToken env
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "api" /: "approve"
    let body_params = "id" =: fullName
    void $ req POST uri (ReqBodyUrlEnc body_params) ignoreResponse uat

-- $messages
--
-- Messages aren't implemented yet.

-- $subreddits
--
-- Subreddits.

-- | Fetch a list of subreddits by their IDs. More efficient than @map
-- getSubreddit@ because it only makes one API call.
getSubreddits :: [ID Subreddit] -> RedditT [Subreddit]
getSubreddits = getThingsByIDs

-- | Fetch a single subreddit given its ID.
getSubreddit :: ID Subreddit -> RedditT Subreddit
getSubreddit = getThingByID

-- | Fetch a list of subreddits by their names. More efficient than @map
-- getSubredditByName@.
getSubredditsByName ::
  -- | List of subreddit names (without the @\/r\/@'s).
  [Text] ->
  RedditT [Subreddit]
getSubredditsByName s_names = withTokenCheck $ do
  env <- ask
  uat <- withUAToken env
  let allNames = T.intercalate "," s_names
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "api" /: "info"
    let req_params = uat <> "sr_name" =: allNames
    response <- req GET uri NoReqBody lbsResponse req_params
    pure (responseBody response)
  psts <- contents <$> throwDecode respBody
  when
    (length psts /= length s_names)
    (fail $ "Reddit response had incorrect length: expected " <> show (length s_names) <> ", found " <> show (length psts))
  pure psts

-- | Fetch a single subreddit by its ID.
getSubredditByName ::
  -- | Subreddit name (without the @\/r\/@).
  Text ->
  RedditT Subreddit
getSubredditByName s_name = do
  srd <- getSubredditsByName [s_name]
  case srd of
    [s] -> pure s
    _ -> fail "Reddit response had incorrect length"

-- $awards
--
-- Awards aren't implemented yet.

-- $streams
--
-- @Stream@s are a popular feature in the Python PRAW library. They are
-- reproduced here as they are useful for accomplishing a variety of bot-related
-- tasks, usually iterating over a list of most recent posts / comments on a
-- subreddit.
--
-- The most general function is @stream@; a usage example is provided in the
-- 'Reddit.Example' module. @postStream@ and @commentStream@ are specialised
-- versions which save a tiny bit of typing.

-- | Available configuration variables for streams.
data StreamSettings = StreamSettings
  { -- | Delay (in seconds) between successive requests when using
    -- [streams](#streams). Reddit says you should not be querying more than 60
    -- times in a minute, so this should not go below 1. Defaults to 5.
    streamsDelay :: Double,
    -- | Number of \'seen\' items to keep in memory when running a stream. If
    -- you are requesting N items at a go, there doesn't appear to be much point
    -- in making this larger than N. N should probably be 100, but this defaults
    -- to 250 to be safe, because I'm not sure if there are weird edge cases.
    streamsStorageSize :: Int
  }

-- | Default stream settings. See 'StreamSettings' for the specification of
-- these values.
defaultStreamSettings :: StreamSettings
defaultStreamSettings =
  StreamSettings
    { streamsDelay = 5,
      streamsStorageSize = 250
    }

-- Helper function.
streamInner :: (Eq a) => StreamSettings -> Q.Queue a -> (t -> a -> RedditT t) -> t -> RedditT [a] -> RedditT ()
streamInner settings queue cb cbInit src = do
  -- `queue` essentially contains the last N items we've seen.
  liftIO $ threadDelay (floorDoubleInt (streamsDelay settings * 1000000))
  items <- src
  let (queue', unique) = Q.merge items queue
  cbUpdated <- foldM cb cbInit unique
  streamInner settings queue' cb cbUpdated src

-- | If you have an action which generates a list of things (with the type
-- @RedditT [a]@), then "stream" turns this an action which
-- executes a callback function on an infinite list of things. It does so by
-- repeatedly fetching the list of either comments or posts.
--
-- Apart from the thing being acted on, the callback function is allowed to also
-- take, as input, some kind of state, and update that state by returning a new
-- value. This allows the user to, for example, keep track of how many things
-- have been seen so far, or perform actions conditionally based on what the
-- stream has previously thrown up.
--
-- You can adjust the frequency with which the stream is refreshed, and
-- secondly, the number of \'seen\' items which are stored in memory. These can
-- be changed using the 'delay' and 'storageSize' fields in the
-- 'StreamSettings' used.
stream ::
  (Eq a) =>
  -- | Settings.
  StreamSettings ->
  -- | A callback function to execute on all things found.
  (state -> a -> RedditT state) ->
  -- | The initial state for the callback function.
  state ->
  -- | The source of things to iterate over.
  RedditT [a] ->
  RedditT ()
stream settings cb cbInit src = do
  first <- src
  streamInner settings (Q.fromList (streamsStorageSize settings) first) cb cbInit src

-- | @stream'@ is a simpler version of @stream@, which accepts a callback that
-- doesn't use state.
stream' ::
  (Eq a) =>
  -- | Settings.
  StreamSettings ->
  -- | A simple callback function which iterates over all things seen.
  (a -> RedditT ()) ->
  -- | The source of things to iterate over.
  RedditT [a] ->
  RedditT ()
stream' settings cb' = stream settings (const cb') ()

-- | Get a stream of new posts on a subreddit.
postStream ::
  -- | Settings.
  StreamSettings ->
  -- | Callback function.
  (state -> Post -> RedditT state) ->
  -- | Initial state for callback.
  state ->
  -- | Subreddit name (without the @\/r\/@).
  Text ->
  RedditT ()
postStream settings cb cbInit sr = stream settings cb cbInit (subredditPosts 100 sr New)

-- | Get a stream of new comments on a subreddit.
--
-- Note that this also includes edited comments (that's not a design choice,
-- it's just how the Reddit API works). If you want to only act on newly posted
-- comments, you can check the @editedTime@ field of the comment in your
-- callback function; however, note that this will still show up as
-- @NeverEdited@ for comments edited within the 3-minute grace period (i.e.
-- \'ninja edits\').
commentStream ::
  -- | Settings.
  StreamSettings ->
  -- | Callback function.
  (state -> Comment -> RedditT state) ->
  -- | Initial state for callback.
  state ->
  -- | Subreddit name (without the @\/r\/@).
  Text ->
  RedditT ()
commentStream settings cb cbInit sr = stream settings cb cbInit (subredditComments 100 sr)
