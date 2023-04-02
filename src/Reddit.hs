{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      : Reddit
-- Description : Functions to interact with the Reddit OAuth2 API
-- Copyright   : (c) Penelope Y. 2023
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
    RedditEnv,
    streamDelay,
    -- | #credentials#

    -- * Authentication with account credentials
    -- $credentials
    Credentials (..),
    withCredentials,

    -- * Comments

    --
    -- $comments
    Reddit.Types.Comment (..),
    getComments,
    getComment,
    addNewComment,
    accountComments,
    subredditComments,

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
import Control.Exception (Exception (..), throwIO)
import Control.Monad (foldM)
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List (union, (\\))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Time.Clock
import GHC.Float.RealFracMethods (floorDoubleInt)
import Network.HTTP.Req
import qualified Reddit.Auth as Auth
import Reddit.Types

-- | Everything required to query the Reddit API.
--
-- This is a record type which includes things that you /might/ want to change
-- (e.g. configuration variables), as well as some things that you really
-- /should not/ change (e.g. the access token). Because of this, not all field
-- accessors are exported; but due to [a bug in
-- Haddock](https://github.com/haskell/haddock/issues/456), the ones that are
-- exported are documented as separate functions instead of \'standalone\' field
-- accessors. However, they /are/ still field accessors, so you can still do
-- things like:
--
-- @
--    env <- 'withCredentials' ...
--    let modifiedEnv = env {'streamDelay' = 2}
-- @
--
-- To be clear, the exported field accessors are: 'streamDelay' only (so far).
data RedditEnv = RedditEnv
  { -- | OAuth2 token contents. Don't modify these.
    token :: ByteString,
    tokenType :: ByteString,
    tokenExpiresAt :: UTCTime,
    tokenScope :: ByteString,
    -- | User-agent. Should be unique.
    userAgent :: ByteString,
    -- | Delay (in seconds) between successive requests when using
    -- [streams](#streams). Reddit says you should not be querying more than 60
    -- times in a minute, so this should not go below 1. Defaults to 5.
    streamDelay :: Double
  }
  deriving (Show)

-- | The type of a Reddit computation.
type RedditT = ReaderT RedditEnv IO

-- | Run a Reddit computation using an @env@ value obtained through
-- authorisation (see [Authentication](#credentials)).
runRedditT :: RedditT a -> RedditEnv -> IO a
runRedditT = runReaderT

-- | @runRedditT'@ is @flip 'runRedditT'@ and is slightly more ergonomic.
runRedditT' :: RedditEnv -> RedditT a -> IO a
runRedditT' = flip runReaderT

-- $credentials
--
-- This section describes the credentials needed for using the Reddit API as a
-- \'script\'.
--
-- In order to obtain these, you will need to:
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

data Credentials = Credentials
  { username :: Text,
    password :: Text,
    clientID :: Text,
    clientSecret :: Text
  }
  deriving (Show)

-- | Exchange user account credentials for a 'RedditEnv', which is required to
-- run all Reddit queries.
withCredentials ::
  Credentials ->
  -- | Your user-agent. [Reddit
  -- says](https://github.com/reddit-archive/reddit/wiki/API) you should use a
  -- unique and identifiable user-agent.
  Text ->
  IO RedditEnv
withCredentials creds ua = do
  (tokenInternal :: Auth.TokenInternal) <- runReq defaultHttpConfig $ do
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

  currentTime <- getCurrentTime
  let seconds = secondsToNominalDiffTime . realToFrac $ tokenInternal._expires_in
  let expires_at = addUTCTime seconds currentTime
  pure $
    RedditEnv
      { token = TE.encodeUtf8 tokenInternal._access_token,
        tokenType = TE.encodeUtf8 tokenInternal._token_type,
        tokenScope = TE.encodeUtf8 tokenInternal._scope,
        tokenExpiresAt = expires_at,
        userAgent = TE.encodeUtf8 ua,
        streamDelay = 5
      }

oauth :: Text
oauth = "oauth.reddit.com"

withUAToken :: RedditEnv -> Option 'Https
withUAToken env = header "user-agent" (userAgent env) <> oAuth2Bearer (token env)

data RedditException = TokenExpiredException deriving (Show)

instance Exception RedditException

-- | TODO: Don't check before using it, check the response headers instead to
-- determine if token has expired
checkTokenValidity :: RedditT ()
checkTokenValidity = do
  expiryTime <- reader tokenExpiresAt
  currentTime <- liftIO getCurrentTime
  when
    (nominalDiffTimeToSeconds (diffUTCTime currentTime expiryTime) > 0)
    (liftIO (throwIO TokenExpiredException))

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
-- two HTTP requests.
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
-- This assumes that the endpoint accepts the 'limit' and 'after' URL-encoded
-- parameters.
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
getListingSingle size aft url in_params = do
  env <- ask
  when (size > 100) (error "getListingSingle: size should be 100 or less. This is a bug, please report it.")
  let params =
        in_params
          <> withUAToken env
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
-- This assumes that the endpoint accepts the 'count' and 'after' URL-encoded
-- parameters.
getListings ::
  (HasID t, FromJSON t) =>
  -- | The number of things to ask for. Must be 100 or fewer.
  Int ->
  -- | The 'after'
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
getThingsByIDs ids = do
  env <- ask
  let fullNames = T.intercalate "," (map mkFullNameFromID ids)
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "api" /: "info"
    let params = withUAToken env <> "id" =: fullNames
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
-- TODO: This uses the @\/api\/info@ endpoint, which (apparently) provides
-- slightly less information than @\/comments\/\<article\>@. For example, this
-- endpoint doesn't provide a list of replies. This is probably worth
-- investigating.
getComments :: [ID Comment] -> RedditT [Comment]
getComments = getThingsByIDs

-- | Fetch a single comment given its ID.
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
addNewComment x body = do
  env <- ask
  let fullName = mkFullNameFromID x
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "api" /: "comment"
    let req_params = withUAToken env
    let body_params = "thing_id" =: fullName <> "text" =: body
    void $ req POST uri (ReqBodyUrlEnc body_params) ignoreResponse req_params

-- | Get the most recent comments by a user.
accountComments ::
  -- | Number of comments to fetch. See [the listings section](#listings) for
  -- an explanation of this parameter.
  Int ->
  -- | Username (without the @\/u\/@).
  Text ->
  RedditT [Comment]
accountComments n uname =
  let uri = (https oauth /: "user" /: uname /: "comments")
   in getListings n Nothing uri mempty

-- | Get the most recent comments on a subreddit.
subredditComments ::
  -- | Number of comments to fetch. See [the listings section](#listings) for
  -- an explanation of this parameter.
  Int ->
  -- | Subreddit name (without the @\/r\/@).
  Text ->
  RedditT [Comment]
subredditComments n sr =
  let uri = https oauth /: "r" /: sr /: "comments"
   in getListings n Nothing uri mempty

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
getAccountByName uname = do
  env <- ask
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "user" /: uname /: "about"
    response <- req GET uri NoReqBody lbsResponse (withUAToken env)
    pure (responseBody response)
  throwDecode respBody

-- $posts
--
-- Fetch the first 25 posts from a given subreddit ordered by the
-- @SubredditSort@ parameter.

data Timeframe = Hour | Day | Week | Month | Year | All

data SubredditSort = Hot | New | Random | Rising | Top Timeframe | Controversial Timeframe

-- | Fetch a list of posts by their IDs. More efficient than @map getPost@
-- because it only makes one API call.
getPosts :: [ID Post] -> RedditT [Post]
getPosts = getThingsByIDs

-- | Fetch a single post given its ID.
getPost :: ID Post -> RedditT Post
getPost = getThingByID

-- | Get the most recent posts by a user.
accountPosts ::
  -- | Number of posts to fetch. See [the listings section](#listings) for an
  -- explanation of this parameter.
  Int ->
  -- | Username (without the @\/u\/@).
  Text ->
  RedditT [Post]
accountPosts n uname = do
  let uri = https oauth /: "user" /: uname /: "submitted"
   in getListings n Nothing uri mempty

-- | Get the posts from the front page of a subreddit.
subredditPosts ::
  -- | Number of posts to fetch. See [the listings section](#listings) for an
  -- explanation of this parameter.
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
getSubredditsByName s_names = do
  env <- ask
  let allNames = T.intercalate "," s_names
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "api" /: "info"
    let params = withUAToken env <> "sr_name" =: allNames
    response <- req GET uri NoReqBody lbsResponse params
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
-- The most general function is @stream@; a usage example is provided at the top
-- of this file. @postStream@ and @commentStream@ are specialised versions which
-- save a tiny bit of typing.

-- Helper function.
streamInner :: (Eq a) => [a] -> (t -> a -> RedditT t) -> t -> RedditT [a] -> RedditT ()
streamInner seen cb cbInit src = do
  delaySeconds <- asks streamDelay
  liftIO $ threadDelay (floorDoubleInt (delaySeconds * 1000000))
  items <- reverse <$> src
  let new = items \\ seen
  cbUpdated <- foldM cb cbInit new
  -- TODO: (seen `union` items) can get infinitely big. We probably want to
  -- prune it when it reaches a certain length. Use some kind of deque?
  streamInner (seen `union` items) cb cbUpdated src

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
-- secondly, the number of items fetched on each request. These can be changed
-- using the 'streamDelay' field of the 'RedditEnv' you are using.
-- The 'Control.Monad.Reader.local' function is particularly helpful here.
stream ::
  (Eq a) =>
  -- | A callback function to execute on all things found.
  (state -> a -> RedditT state) ->
  -- | The initial state for the callback function.
  state ->
  -- | The source of things to iterate over.
  RedditT [a] ->
  RedditT ()
stream cb cbInit src = do
  first <- src
  streamInner first cb cbInit src

-- | @stream'@ is a simpler version of @stream@, which accepts a callback that
-- doesn't use state.
stream' ::
  (Eq a) =>
  -- | Whether to ignore the first request.
  (a -> RedditT ()) ->
  -- | The source of things to iterate over.
  RedditT [a] ->
  RedditT ()
stream' cb' = stream (const cb') ()

-- | Get a stream of new posts on a subreddit.
postStream ::
  -- | Callback function.
  (state -> Post -> RedditT state) ->
  -- | Initial state for callback.
  state ->
  -- | Subreddit name (without the @\/r\/@).
  Text ->
  RedditT ()
postStream cb cbInit sr = stream cb cbInit (subredditPosts 100 sr New)

-- | Get a stream of new comments on a subreddit. Note that this also includes
-- edited comments (that's not a design choice, it's just how the Reddit API
-- works).
commentStream ::
  -- | Callback function.
  (state -> Comment -> RedditT state) ->
  -- | Initial state for callback.
  state ->
  -- | Subreddit name (without the @\/r\/@).
  Text ->
  RedditT ()
commentStream cb cbInit sr = stream cb cbInit (subredditComments 100 sr)
