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
-- It is not currently fully-fledged, but contains enough stuff to let you make
-- a bot that replies to certain phrases found in comments / posts (which is a
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
    RedditEnv,
    runRedditT,
    revokeToken,

    -- * Exceptions
    -- $exceptions
    RedditException (..),

    -- ** Manual environment management
    -- $env_management
    getTokenFromEnv,
    mkEnvFromToken,

    -- * Authentication #authentication#
    -- $auth
    Auth.Credentials (..),
    authenticate,
    Auth.AuthUrlParams (..),
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
    Reddit.Types.Account (..),
    myAccount,
    getAccounts,
    getAccount,
    getAccountByName,

    -- * Posts
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
    Reddit.Types.Message (..),

    -- * Subreddits
    Reddit.Types.Subreddit (..),
    getSubreddits,
    getSubreddit,
    getSubredditsByName,
    getSubredditByName,

    -- * Awards
    Reddit.Types.Award (..),

    -- * Streams #streams#
    -- $streams
    Reddit.Stream.StreamSettings (..),
    Reddit.Stream.defaultStreamSettings,
    Reddit.Stream.stream,
    Reddit.Stream.stream',

    -- * Listings #listings#
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
import Control.Monad (void, when)
import Control.Monad.Catch (MonadThrow (..))
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
import Network.HTTP.Req
import qualified Reddit.Auth as Auth
import Reddit.Stream
import Reddit.Types

-- | Revoke an access token contained in a @RedditEnv@, rendering it unusable.
-- If you want to continue performing queries after this, you will need to
-- generate a new @RedditEnv@.
--
-- It is generally good practice to call this function when you are done using a
-- token.
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

-- | Convenience function to generate HTTP headers required for basic
-- authentication.
withUAToken :: (MonadIO m) => RedditEnv -> m (Option 'Https)
withUAToken env = do
  t <- liftIO $ R.readIORef (envTokenRef env)
  pure $ header "user-agent" (envUserAgent env) <> oAuth2Bearer (Auth.token t)

-- $env_management
-- For more complicated use cases, you will almost certainly have to perform
-- more manual management of the @RedditEnv@ value. Specifically, you will want
-- to extract tokens from envs (for example, to store them in a database), and
-- re-generate new envs from existing tokens.
--
-- The functions in this section aim to make this process smoother.

-- | Extract the current OAuth2 token being used.
getTokenFromEnv :: RedditEnv -> IO Auth.Token
getTokenFromEnv env = do
  let tRef = envTokenRef env
  R.readIORef tRef

-- | Construct a new RedditEnv
mkEnvFromToken ::
  Auth.Token ->
  -- | User-agent
  Text ->
  IO RedditEnv
mkEnvFromToken token userAgent = do
  tokenRef <- R.newIORef token
  pure $
    RedditEnv
      { envTokenRef = tokenRef,
        envCredentials = Auth.NoCredentials,
        envUserAgent = TE.encodeUtf8 userAgent
      }

-- $auth
-- The first step to querying the Reddit API is to authenticate with Reddit.
-- (Technically, you don't /have/ to; however, you will be subjected to a much
-- lower rate limit. This library does not support unauthenticated usage of the
-- Reddit API.)
--
-- The method in which you authenticate will depend on the type of application
-- you are building.
--
-- * Most Reddit bots, or scripts, only require you to authenticate as a single
-- user (which may be a bot account). In this case, you can simply log in with
-- your account credentials, using the 'Auth.OwnerCredentials' constructor. If
-- you are not sure what you are doing, this is probably what you want.
--
-- * If you are instead building an application which is meant to be used by
-- other people (e.g. a web app to let visitors analyse their posts), then you
-- need to authenticate via the 'code grant' method, using the
-- 'Auth.CodeGrantCredentials' constructor.
--
-- In either case, you need to construct the appropriate 'Auth.Credentials' and
-- pass them to the 'authenticate' function, which will give you a 'RedditEnv'
-- value that you can then use to query Reddit.

-- | Once you have set up your 'Auth.Credentials', you can use this function to
-- exchange them for a token contained inside a 'RedditEnv', which can then be
-- used to perform all Reddit queries.
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
        envCredentials = creds,
        envUserAgent = uaBS
      }

-- | This function is not exported, but is used when the access token has to be
-- updated (i.e. when it expires). It updates the contents of the IORefs in the
-- RedditEnv with an updated token.
reauthenticate :: RedditEnv -> IO ()
reauthenticate env = do
  case envCredentials env of
    c@(Auth.OwnerCredentials {..}) -> do
      token <- Auth.getToken c (envUserAgent env)
      R.writeIORef (envTokenRef env) token
    _ -> error "Not supported"

-- | Perform a RedditT IO action, but before running it, check the validity of the
-- existing token and reauthenticate if it has expired already.
--
-- Generally, we only need to use this in functions which are actually querying
-- a Reddit API endpoint (it doesn't need to be indiscriminately applied to all
-- functions with a RedditT IO type).
withTokenCheck :: (MonadIO m) => RedditT m a -> RedditT m a
withTokenCheck action = do
  env <- ask
  t <- liftIO $ R.readIORef (envTokenRef env)
  let expiryTime = Auth.tokenExpiresAt t
  currentTime <- liftIO getCurrentTime
  when
    (nominalDiffTimeToSeconds (diffUTCTime currentTime expiryTime) > 0)
    (liftIO $ reauthenticate env)
  action

oauthUri :: Text
oauthUri = "oauth.reddit.com"

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
--    100 if you're creating a [stream](#g:streams).
-- 2. If you just want 'as many as possible', put 1000.
-- 3. Don't bother putting anything larger than 1000.

-- | Internal function to fetch n <= 100 results from a given URI. Assumes that
-- the listing will be homogeneous.
--
-- This assumes that the endpoint accepts the \'limit\' and \'after\'
-- URL-encoded parameters.
getListingSingle ::
  (MonadIO m, HasID t, FromJSON t) =>
  -- | The number of things to ask for. Should really be 100 or fewer.
  Int ->
  -- | The value of @after@ to pass in the query.
  Maybe Text ->
  -- | The URL to query
  Url 'Https ->
  -- | URL-encoded params. Use @mempty@ if not needed.
  Option 'Https ->
  -- | The things, plus the \'after\' field returned by Reddit.
  RedditT m ([t], Maybe Text)
getListingSingle size aft url in_params = withTokenCheck $ do
  env <- ask
  uat <- withUAToken env
  when
    (size > 100)
    (throwIOApi "getListingSingle: size should be 100 or less. This is a bug, please report it.")
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
  case eitherDecode respBody of
    Left e -> throwIOJson e
    Right listing -> pure (contents listing, after listing)

-- | Internal function to fetch n <= 1000 results from a given URI. Assumes that
-- the returned listing will be homogeneous.
--
-- This assumes that the endpoint accepts the \'count\' and \'after\'
-- URL-encoded parameters.
getListings ::
  (MonadIO m, HasID t, FromJSON t) =>
  -- | The number of things to ask for. Must be 100 or fewer.
  Int ->
  -- | The value of \'after\'.
  Maybe Text ->
  -- | The URL to query
  Url 'Https ->
  -- | URL-encoded params. Use @mempty@ if not needed.
  Option 'Https ->
  -- | All the things, concatenated into a single list.
  RedditT m [t]
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
getThingsByIDs :: (MonadIO m, HasID t, FromJSON t) => [ID t] -> RedditT m [t]
getThingsByIDs ids = withTokenCheck $ do
  env <- ask
  uat <- withUAToken env
  let fullNames = T.intercalate "," (map mkFullNameFromID ids)
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauthUri /: "api" /: "info"
    let params = uat <> "id" =: fullNames
    response <- req GET uri NoReqBody lbsResponse params
    pure (responseBody response)
  case contents <$> eitherDecode respBody of
    Left e -> throwIOJson e
    Right ct -> do
      when
        (length ct /= length ids)
        (throwIOApi $ "Reddit response had incorrect length: expected " <> show (length ids) <> ", found " <> show (length ct))
      pure ct

getThingByID :: (MonadIO m, HasID t, FromJSON t) => ID t -> RedditT m t
getThingByID its_id = do
  thing <- getThingsByIDs [its_id]
  case thing of
    [t] -> pure t
    _ -> throwIOApi "Reddit response had incorrect length"

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
getComments :: (MonadIO m) => [ID Comment] -> RedditT m [Comment]
getComments = getThingsByIDs

-- | Fetch a single comment given its ID. The same caveat described for
-- 'getComments' also applies here.
getComment :: (MonadIO m) => ID Comment -> RedditT m Comment
getComment = getThingByID

-- | Add a new comment as a reply to an existing post or comment.
--
-- The 'CanCommentOn' constraint ensures that you can only reply to comments and
-- posts.
addNewComment ::
  (CanCommentOn a, MonadIO m) =>
  -- | The ID of the thing being replied to.
  ID a ->
  -- | The contents of the comment (in Markdown)
  Text ->
  RedditT m ()
addNewComment x body = withTokenCheck $ do
  env <- ask
  uat <- withUAToken env
  let fullName = mkFullNameFromID x
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauthUri /: "api" /: "comment"
    let body_params = "thing_id" =: fullName <> "text" =: body
    void $ req POST uri (ReqBodyUrlEnc body_params) ignoreResponse uat

-- | Get the most recent comments by a user.
accountComments ::
  (MonadIO m) =>
  -- | Number of comments to fetch (maximum 1000). See [the listings
  -- section](#g:listings) for an explanation of this parameter.
  Int ->
  -- | Username (without the @\/u\/@).
  Text ->
  RedditT m [Comment]
accountComments n uname =
  let uri = (https oauthUri /: "user" /: uname /: "comments")
   in getListings n Nothing uri mempty

-- | Get the most recent comments on a subreddit.
subredditComments ::
  (MonadIO m) =>
  -- | Number of comments to fetch (maximum 1000). See [the listings
  -- section](#g:listings) for an explanation of this parameter.
  Int ->
  -- | Subreddit name (without the @\/r\/@).
  Text ->
  RedditT m [Comment]
subredditComments n sr =
  let uri = https oauthUri /: "r" /: sr /: "comments"
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
getPostWithComments :: (MonadIO m) => ID Post -> RedditT m (Post, [CommentTree])
getPostWithComments (PostID p) = withTokenCheck $ do
  env <- ask
  uat <- withUAToken env
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauthUri /: "comments" /: p
    response <- req GET uri NoReqBody lbsResponse uat
    pure (responseBody response)
  -- Reddit returns a JSON array where the first item is a listing containing
  -- the post, and the second a listing containing the comments. Thankfully, the
  -- default implementation for FromJSON (a, b) does exactly this.
  case eitherDecode respBody of
    Left e -> throwIOJson e
    Right (postListing, cmtListing) -> do
      case contents postListing of
        [p] -> pure (p, contents cmtListing)
        _ -> throwIOApi "Expected one post, got many"

-- | Fetch unshown comments on a post.
getMoreChildren :: (MonadIO m) => ID Post -> [ID Comment] -> RedditT m [MoreChildren]
getMoreChildren pid cids =
  if null cids
    then pure []
    else withTokenCheck $ do
      env <- ask
      uat <- withUAToken env
      respBody <- liftIO $ runReq defaultHttpConfig $ do
        let uri = https oauthUri /: "api" /: "morechildren"
        let query_params =
              uat
                <> "api_type" =: ("json" :: Text)
                <> "link_id" =: mkFullNameFromID pid
                <> "children" =: T.intercalate "," (map unCommentID cids)
        response <- req GET uri NoReqBody lbsResponse query_params
        pure (responseBody response)
      -- The actual data we want is nested a few levels down.
      case eitherDecode respBody of
        Left e -> throwIOJson e
        Right (Object v) ->
          case parse (\v -> v .: "json" >>= (.: "data") >>= (.: "things")) v of
            Error err -> throwIOJson err
            -- 'things' is the actual array of stuff we want
            Success (things :: Value) -> case parse parseJSON things of
              Error err' -> throwIOJson err'
              Success result -> pure result
        Right _ -> throwIOApi "getMoreChildren: expected JSON object"

-- | Expand the first instance of @MoreComments@ in a tree. The required post ID
-- is that of the post for which you are expanding comments.
expandTree :: (MonadIO m) => ID Post -> [CommentTree] -> RedditT m [CommentTree]
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
expandTreeFully :: (MonadIO m) => ID Post -> [CommentTree] -> RedditT m [CommentTree]
expandTreeFully pid trees = case getFirstMore trees of
  Nothing -> pure trees
  Just _ -> do
    trees' <- expandTree pid trees
    let sz = sum (map treeSize trees)
    let sz' = sum (map treeSize trees')
    expandTreeFully pid trees'

-- | Fetch the account of the currently logged-in user.
myAccount :: (MonadIO m) => RedditT m Account
myAccount = withTokenCheck $ do
  env <- ask
  uat <- withUAToken env
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauthUri /: "api" /: "v1" /: "me"
    response <- req GET uri NoReqBody lbsResponse uat
    pure (responseBody response)
  case eitherDecode respBody of
    Left e -> throwIOJson e
    Right a -> pure a

-- | Fetch a list of accounts given their IDs. More efficient than @map
-- getAccount@ because it only makes one API call.
getAccounts :: (MonadIO m) => [ID Account] -> RedditT m [Account]
getAccounts = getThingsByIDs

-- | Fetch a single account given its ID.
getAccount :: (MonadIO m) => ID Account -> RedditT m Account
getAccount = getThingByID

-- | Fetch details about a named user.
getAccountByName ::
  (MonadIO m) =>
  -- | Username (without the @\/u\/@).
  Text ->
  RedditT m Account
getAccountByName uname = withTokenCheck $ do
  env <- ask
  uat <- withUAToken env
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauthUri /: "user" /: uname /: "about"
    response <- req GET uri NoReqBody lbsResponse uat
    pure (responseBody response)
  case eitherDecode respBody of
    Left e -> throwIOJson e
    Right a -> pure a

-- | The timeframe over which to get top or most controversial posts.
data Timeframe = Hour | Day | Week | Month | Year | All

-- | Sorting order for posts in a subreddit.
data SubredditSort = Hot | New | Random | Rising | Top Timeframe | Controversial Timeframe

-- | Fetch a list of posts by their IDs. More efficient than @map getPost@
-- because it only makes one API call.
getPosts :: (MonadIO m) => [ID Post] -> RedditT m [Post]
getPosts = getThingsByIDs

-- | Fetch a single post given its ID.
--
-- If you want to fetch a post together with its comments, you can use
-- 'getPostWithComments'.
getPost :: (MonadIO m) => ID Post -> RedditT m Post
getPost = getThingByID

-- | Get the most recent posts by a user.
accountPosts ::
  (MonadIO m) =>
  -- | Number of posts to fetch (maximum 1000). See [the listings
  -- section](#g:listings) for an explanation of this parameter.
  Int ->
  -- | Username (without the @\/u\/@).
  Text ->
  RedditT m [Post]
accountPosts n uname = do
  let uri = https oauthUri /: "user" /: uname /: "submitted"
   in getListings n Nothing uri mempty

-- | Get the posts from the front page of a subreddit, with the ordering
-- specified in the @SubredditSort@ argument.
subredditPosts ::
  (MonadIO m) =>
  -- | Number of posts to fetch (maximum 1000). See [the listings
  -- section](#g:listings) for an explanation of this parameter.
  Int ->
  -- | Subreddit name (without the @\/r\/@).
  Text ->
  -- | Sort type for the subreddit posts. Entirely analogous to the options when
  -- browsing Reddit on the web.
  SubredditSort ->
  RedditT m [Post]
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
  let uri = https oauthUri /: "r" /: sr /: endpoint
   in getListings n Nothing uri tf_params

-- | Edit the contents of a comment or a text post. You must be authenticated as
-- the person who posted it.
edit :: (CanCommentOn a, MonadIO m) => ID a -> Text -> RedditT m ()
edit id newBody = withTokenCheck $ do
  let fullName = mkFullNameFromID id
  env <- ask
  uat <- withUAToken env
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauthUri /: "api" /: "editusertext"
    let body_params = "thing_id" =: fullName <> "text" =: newBody
    void $ req POST uri (ReqBodyUrlEnc body_params) ignoreResponse uat

-- | Delete a post or a comment. You must be authenticated as the person who
-- posted it.
--
-- If you want to remove a post or a comment on a subreddit you moderate, use
-- 'remove' instead.
delete :: (CanCommentOn a, MonadIO m) => ID a -> RedditT m ()
delete id = withTokenCheck $ do
  let fullName = mkFullNameFromID id
  env <- ask
  uat <- withUAToken env
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauthUri /: "api" /: "del"
    let body_params = "id" =: fullName
    void $ req POST uri (ReqBodyUrlEnc body_params) ignoreResponse uat

-- | As a moderator, remove a post or a comment. This is the inverse of
-- 'approve'.
remove ::
  (CanCommentOn a, MonadIO m) =>
  ID a ->
  -- | Whether the thing being removed is spam.
  Bool ->
  RedditT m ()
remove id isSpam = withTokenCheck $ do
  let fullName = mkFullNameFromID id
  let spam :: Text = if isSpam then "true" else "false"
  env <- ask
  uat <- withUAToken env
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauthUri /: "api" /: "remove"
    let body_params = "id" =: fullName <> "spam" =: spam
    void $ req POST uri (ReqBodyUrlEnc body_params) ignoreResponse uat

-- | As a moderator, approve a post or a comment. This is the inverse of
-- 'remove'.
approve :: (CanCommentOn a, MonadIO m) => ID a -> RedditT m ()
approve id = withTokenCheck $ do
  let fullName = mkFullNameFromID id
  env <- ask
  uat <- withUAToken env
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauthUri /: "api" /: "approve"
    let body_params = "id" =: fullName
    void $ req POST uri (ReqBodyUrlEnc body_params) ignoreResponse uat

-- | Fetch a list of subreddits by their IDs. More efficient than @map
-- getSubreddit@ because it only makes one API call.
getSubreddits :: (MonadIO m) => [ID Subreddit] -> RedditT m [Subreddit]
getSubreddits = getThingsByIDs

-- | Fetch a single subreddit given its ID.
getSubreddit :: (MonadIO m) => ID Subreddit -> RedditT m Subreddit
getSubreddit = getThingByID

-- | Fetch a list of subreddits by their names. More efficient than @map
-- getSubredditByName@.
getSubredditsByName ::
  (MonadIO m) =>
  -- | List of subreddit names (without the @\/r\/@'s).
  [Text] ->
  RedditT m [Subreddit]
getSubredditsByName s_names = withTokenCheck $ do
  env <- ask
  uat <- withUAToken env
  let allNames = T.intercalate "," s_names
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauthUri /: "api" /: "info"
    let req_params = uat <> "sr_name" =: allNames
    response <- req GET uri NoReqBody lbsResponse req_params
    pure (responseBody response)
  case eitherDecode respBody of
    Left e -> throwIOJson e
    Right psts -> do
      when
        (length psts /= length s_names)
        (throwIOApi $ "Reddit response had incorrect length: expected " <> show (length s_names) <> ", found " <> show (length psts))
      pure psts

-- | Fetch a single subreddit by its ID.
getSubredditByName ::
  (MonadIO m) =>
  -- | Subreddit name (without the @\/r\/@).
  Text ->
  RedditT m Subreddit
getSubredditByName s_name = do
  srd <- getSubredditsByName [s_name]
  case srd of
    [s] -> pure s
    _ -> throwIOApi "Reddit response had incorrect length"

-- $streams
--
-- @Stream@s are a popular feature in the Python PRAW library. They are
-- reproduced here as they are useful for accomplishing a variety of bot-related
-- tasks, usually iterating over a list of most recent posts / comments on a
-- subreddit.
--
-- A simple usage example is provided in the 'Reddit.Example' module.
