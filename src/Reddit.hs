{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Reddit
  ( -- * Users

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
    subredditComments,

    -- * Streams

    --
    -- $streams
    stream,
    stream',
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
import Data.List (union, (\\))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Network.HTTP.Req
import Reddit.Auth (RedditEnv (..))
import Reddit.Types

oauth :: Text
oauth = "oauth.reddit.com"

withUAToken :: RedditEnv -> Option 'Https
withUAToken env = header "user-agent" (envUserAgent env) <> oAuth2Bearer (envToken env)

data RedditException = TokenExpiredException deriving (Show)

instance Exception RedditException

-- | TODO: Don't check before using it, check the response headers instead to
-- determine if token has expired
checkTokenValidity :: ReaderT RedditEnv IO ()
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
user :: Text -> ReaderT RedditEnv IO ByteString
user username = do
  env <- ask
  checkTokenValidity
  liftIO $ runReq defaultHttpConfig $ do
    let uri = https oauth /: "user" /: username /: "about"
    response <- req GET uri NoReqBody bsResponse (withUAToken env)
    pure $ responseBody response

-- $posts
--
-- Fetch the first 25 posts from a given subreddit ordered by the
-- @SubredditSort@ parameter.

data Timeframe = Hour | Day | Week | Month | Year | All

data SubredditSort = Hot | New | Random | Rising | Top Timeframe | Controversial Timeframe

-- | Get the first 25 posts on a subreddit
subredditPosts :: Text -> SubredditSort -> ReaderT RedditEnv IO [Post]
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
subredditComments :: Text -> ReaderT RedditEnv IO [Comment]
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
-- 'Streams' are a popular feature in the Python PRAW library; they are
-- reproduced here as they are useful for accomplishing a variety of bot-related
-- tasks.
--
-- You're most likely to use the @stream@ function. Here are a couple of usage
-- examples:
--
-- TODO rewrite this T_T

-- Helper function.
streamInner :: Eq a => [a] -> ReaderT RedditEnv IO [a] -> (t -> a -> IO t) -> t -> ReaderT RedditEnv IO ()
streamInner seen src cb cbInit = do
  liftIO $ threadDelay 5000000
  items <- src
  let new = items \\ seen
  cbUpdated <- liftIO $ foldM cb cbInit new
  streamInner (seen `union` items) src cb cbUpdated

-- | TODO rewrite this documentation T_T
stream :: Eq a
       => Bool  -- ^ Whether to ignore the first batch of requests.
       -> ReaderT RedditEnv IO [a]  -- ^ The source of things to iterate over.
       -> (t -> a -> IO t)  -- A callback function to execute on all things found.
       -> t  -- ^ The initial state for the callback function.
       -> ReaderT RedditEnv IO ()
stream ignoreExisting src cb cbInit = do
  first <- src
  let seen = if ignoreExisting then first else []
  streamInner seen src cb cbInit

-- | TODO rewrite this documentation T_T
stream' :: Eq a
        => Bool  -- ^ Whether to ignore the first batch of requests.
        -> ReaderT RedditEnv IO [a]  -- ^ The source of things to iterate over.
        -> (a -> IO ())  -- A callback function to execute on all things found.
        -> ReaderT RedditEnv IO ()
stream' ignoreExisting src cb' =
  stream ignoreExisting src (const cb') ()
