{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : Reddit
Description : Functions to interact with the Reddit OAuth2 API
Copyright   : (c) Penelope Y. 2023
License     : MIT
Maintainer  : penelopeysm@gmail.com
Stability   : experimental

@reddit-oauth2@ provides a collection of functions to query the Reddit OAuth2 API.

It is currently very primitive, but contains enough stuff to let you make a bot
that replies to certain phrases found in comments / posts (which is a very
common use case).

It is probably easiest to demonstrate this with an example. One such example is
provided in @src/Reddit/Example.hs@. When run, this script will log each new
comment posted in \/r\/haskell to standard output, scans it for the phrase
@"Haskell is great!!!!!!!!!"@, and if that is found, replies to them with
@"Indeed, it is!"@.

(Even though I don't think many people post that /exact/
phrase on \/r\/haskell, I put in 9 exclamation marks because I don't want to be
morally responsible for somebody gratuitously importing it.)
-}

module Reddit
  ( -- * Types
    RedditEnv,
    RedditT (..),
    runRedditT,
    runRedditT',
    -- * Authentication with account credentials

    -- $credentials
    Credentials (..),
    withCredentials,

    -- * Users

    --
    -- $users
    user,

    -- * Posts

    --
    -- $posts
    Reddit.Types.Post (..),
    Timeframe (..),
    SubredditSort (..),
    subredditPosts,

    -- * Comments

    --
    -- $comments
    Reddit.Types.Comment (..),
    addNewComment,
    subredditComments,

    -- * Streams

    --
    -- $streams
    stream,
    stream',
    postStream,
    commentStream
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
import Data.Time.Clock
import Network.HTTP.Req
import qualified Reddit.Auth as Auth
import Reddit.Types

-- | Everything required to query the Reddit API.
data RedditEnv = RedditEnv
  { envToken :: ByteString,
    envTokenType :: ByteString,
    envTokenExpiresAt :: UTCTime,
    envTokenScope :: ByteString,
    envUserAgent :: ByteString
  }
  deriving (Show)

-- | The type of a Reddit computation.
type RedditT = ReaderT RedditEnv IO

-- | Run a Reddit computation using an @env@ value obtained through
-- authorisation (see "Reddit.Auth").
runRedditT :: RedditT a -> RedditEnv -> IO a
runRedditT = runReaderT

-- | @runRedditT'@ is @flip runRedditT@ and is often more ergonomic.
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

-- | Exchange user account credentials for a RedditEnv, which is required to run
-- all Reddit queries.
withCredentials :: Credentials
                -> Text  -- ^ Your user-agent. Reddit says you should use a unique and identifiable user-agent.
                -> IO RedditEnv
withCredentials creds userAgent = do
  (tokenInternal :: Auth.TokenInternal) <- runReq defaultHttpConfig $ do
    let uri = https "www.reddit.com" /: "api" /: "v1" /: "access_token"
    let body =
          ReqBodyUrlEnc
            ( "grant_type"
                =: ("password" :: Text)
                <> "username"
                =: username creds
                <> "password"
                =: password creds
            )
    let options = basicAuth (TE.encodeUtf8 (clientID creds)) (TE.encodeUtf8 (clientSecret creds))
    response <- req POST uri body lbsResponse options
    throwDecode (responseBody response)

  currentTime <- getCurrentTime
  let seconds = secondsToNominalDiffTime . realToFrac $ tokenInternal._expires_in
  let expires_at = addUTCTime seconds currentTime
  pure $
    RedditEnv
      { envToken = TE.encodeUtf8 tokenInternal._access_token,
        envTokenType = TE.encodeUtf8 tokenInternal._token_type,
        envTokenScope = TE.encodeUtf8 tokenInternal._scope,
        envTokenExpiresAt = expires_at,
        envUserAgent = TE.encodeUtf8 userAgent
      }

oauth :: Text
oauth = "oauth.reddit.com"

withUAToken :: RedditEnv -> Option 'Https
withUAToken env = header "user-agent" (envUserAgent env) <> oAuth2Bearer (envToken env)

data RedditException = TokenExpiredException deriving (Show)

instance Exception RedditException

-- | TODO: Don't check before using it, check the response headers instead to
-- determine if token has expired
checkTokenValidity :: RedditT ()
checkTokenValidity = do
  expiryTime <- reader envTokenExpiresAt
  currentTime <- liftIO getCurrentTime
  when
    (nominalDiffTimeToSeconds (diffUTCTime currentTime expiryTime) > 0)
    (liftIO (throwIO TokenExpiredException))

-- $users
--
-- Fetch a user.
-- TODO: Implement the right type.

-- | Get information about a user
user :: Text -> RedditT ByteString
user username = do
  env <- ask
  checkTokenValidity
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "user" /: username /: "about"
    response <- req GET uri NoReqBody bsResponse (withUAToken env)
    pure $ responseBody response

-- $comments
-- Blah.

-- | Add a new comment as a reply to an existing post or comment.
addNewComment ::
  -- | This constraint ensures that you can only reply to comments and posts.
  CanCommentOn a =>
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

-- $posts
--
-- Fetch the first 25 posts from a given subreddit ordered by the
-- @SubredditSort@ parameter.

data Timeframe = Hour | Day | Week | Month | Year | All

data SubredditSort = Hot | New | Random | Rising | Top Timeframe | Controversial Timeframe

-- | Get the first 25 posts on a subreddit
subredditPosts :: Text -> SubredditSort -> RedditT [Post]
subredditPosts sr sort = do
  env <- ask
  checkTokenValidity
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
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "r" /: sr /: endpoint
    response <- req GET uri NoReqBody lbsResponse (withUAToken env <> tf_params)
    pure (responseBody response)
  posts <$> throwDecode respBody

-- | Get the most recent 25 comments on a subreddit. This endpoint is
-- undocumented!
subredditComments :: Text -> RedditT [Comment]
subredditComments sr = do
  env <- ask
  checkTokenValidity
  respBody <- liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "r" /: sr /: "comments"
    response <- req GET uri NoReqBody lbsResponse (withUAToken env)
    pure (responseBody response)
  comments <$> throwDecode respBody

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
streamInner :: Eq a => [a] -> (t -> a -> RedditT t) -> t -> RedditT [a] -> RedditT ()
streamInner seen cb cbInit src = do
  liftIO $ threadDelay 5000000
  items <- src
  let new = items \\ seen
  cbUpdated <- foldM cb cbInit new
  streamInner (seen `union` items) cb cbUpdated src

-- | If you have an action which generates a list of things (with the type
-- @RedditT [a]@), then @stream@ turns this an action which
-- executes a callback function on an infinite list of things. It does so by
-- repeatedly fetching the list of things (every five seconds). Here, \'things\'
-- refers to comments, posts, and so on.
--
-- Apart from the thing being acted on, the callback function is allowed to also
-- take, as input, some kind of state, and update that state by returning a new
-- value. This allows the user to, for example, keep track of how many things
-- have been seen so far, or perform actions conditionally based on what the
-- stream has previously thrown up.
stream ::
  Eq a =>
  -- | Whether to ignore the things found in the first request. Since the first
  -- request is run when the stream is started, this essentially amounts to
  -- ignoring everything posted /before/ the stream is started. You will most
  -- likely want this to be @True@.
  Bool ->
  -- | A callback function to execute on all things found.
  (state -> a -> RedditT state) ->
  -- | The initial state for the callback function.
  state ->
  -- | The source of things to iterate over.
  RedditT [a] ->
  RedditT ()
stream ignoreExisting cb cbInit src = do
  first <- src
  let seen = if ignoreExisting then first else []
  streamInner seen cb cbInit src

-- | @stream'@ is a simpler version of @stream@, which accepts a callback that
-- doesn't use state.
stream' ::
  Eq a =>
  -- | Whether to ignore the first request.
  Bool ->
  -- | A callback function to execute on all things found.
  (a -> RedditT ()) ->
  -- | The source of things to iterate over.
  RedditT [a] ->
  RedditT ()
stream' ignoreExisting cb' =
  stream ignoreExisting (const cb') ()

-- | Get a stream of new posts on a subreddit.
postStream ::
  -- | Whether to ignore the first request.
  Bool ->
  -- | Callback function.
  (state -> Post -> RedditT state) ->
  -- | Initial state for callback.
  state ->
  -- | Subreddit name.
  Text ->
  RedditT ()
postStream ignoreExisting cb cbInit sr = stream ignoreExisting cb cbInit (subredditPosts sr New)

-- | Get a stream of new comments on a subreddit. Note that this also includes
-- edited comments (that's not a design choice, it's just how the Reddit API
-- works).
commentStream ::
  -- | Whether to ignore the first request.
  Bool ->
  -- | Callback function.
  (state -> Comment -> RedditT state) ->
  -- | Initial state for callback.
  state ->
  -- | Subreddit name.
  Text ->
  RedditT ()
commentStream ignoreExisting cb cbInit sr = stream ignoreExisting cb cbInit (subredditComments sr)
