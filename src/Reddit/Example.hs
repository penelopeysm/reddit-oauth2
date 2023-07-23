{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Reddit.Example
-- Description : An example of a simple Reddit bot built using this library
-- Copyright   : (c) Penelope Yong 2023
-- License     : MIT
-- Maintainer  : penelopeysm@gmail.com
-- Stability   : experimental
--
-- When run, this script will log each new comment posted in @\/r\/haskell@ to
-- standard output, scans it for the phrase @"Haskell is great!!!!!!!!!"@, and
-- if that is found, replies to them with @"Indeed, it is!"@. (I put in 9
-- exclamation marks because I don't want to be morally responsible for somebody
-- gratuitously importing it, running it, and spamming the sub.)
--
-- Anyway, you should look at the source code for this module instead of the
-- built Haddocks. It contains many explanatory comments.
module Reddit.Example where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Reddit
import System.Environment (getEnv)
import System.IO (stderr)

-- | 'System.Environment.getEnv' but acts on 'Data.Text.Text' values.
getEnvAsText :: String -> IO T.Text
getEnvAsText = fmap T.pack . getEnv

-- | An example of a callback function acting on comments. It takes an 'Int'
-- input state, the comment it sees, and does some kind of action on Reddit that
-- ultimately returns another 'Int'. The returned value will be used as the
-- input the next time the callback is executed, i.e. on the next comment.
replyIfHaskellGreat :: Int -> Comment -> RedditT IO Int
replyIfHaskellGreat count cmt = do
  -- Print details about the comment on standard output
  mapM_
    (liftIO . T.putStrLn)
    [ "",
      "Found new comment!",
      "By    : /u/" <> commentAuthor cmt,
      "Link  : " <> commentUrl cmt,
      "Text  : " <> commentBody cmt,
      T.pack ("Comments seen so far: " <> show count)
    ]
  let triggerText = "Haskell is great!!!!!!!!"
  let replyText = "Indeed, it is!"
  -- Check whether it contains the trigger text, and reply if necessary
  when (triggerText `T.isInfixOf` commentBody cmt) $ do
    liftIO $ T.putStrLn "Replying to it..."
    addNewComment (commentId cmt) replyText
  -- Increment the count and return
  pure (count + 1)

-- | Run the bot.
main :: IO ()
main = do
  -- Read in your secrets from environment variables (or wherever you like).
  username <- getEnvAsText "REDDIT_USERNAME"
  password <- getEnvAsText "REDDIT_PASSWORD"
  clientID <- getEnvAsText "REDDIT_ID"
  clientSecret <- getEnvAsText "REDDIT_SECRET"
  let userAgent = "your user-agent here"

  -- Get the RedditEnv term needed to run Reddit queries by authenticating.
  let creds =
        OwnerCredentials
          { ownerUsername = username,
            ownerPassword = password,
            ownerClientId = clientID,
            ownerClientSecret = clientSecret
          }
  env <- authenticate creds userAgent

  -- Run the bot, refreshing every 5 seconds (the default).
  runRedditT env $
    stream
      defaultStreamSettings             -- Stream configuration.
      replyIfHaskellGreat               -- The action to run on each thing (a callback).
      1                                 -- The initial state passed to the callback.
      id                                -- Convert m [a] to IO a. Here, m is IO so we just use id.
      (subredditComments 100 "haskell") -- A RedditT m [a] action which generates the things we act on.
