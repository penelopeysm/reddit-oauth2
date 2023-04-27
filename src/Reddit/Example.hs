{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Reddit.Example
-- Description : An example of a simple Reddit bot built using this library
-- Copyright   : (c) Penelope Y. 2023
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
getEnvAsText :: T.Text -> IO T.Text
getEnvAsText = fmap T.pack . getEnv . T.unpack

-- | An example of a callback function acting on comments. It takes an 'Int'
-- input state, the comment it sees, and does some kind of action on Reddit that
-- ultimately returns another 'Int'. The returned value will be used as the
-- input the next time the callback is executed, i.e. on the next comment.
replyIfHaskellGreat :: Int -> Comment -> RedditT Int
replyIfHaskellGreat count cmt = do
  -- Print details about the comment on standard output
  let p = liftIO . T.putStrLn
  p ""
  p "Found new comment!"
  p $ "By    : /u/" <> cmt.author
  p $ "Link  : " <> cmt.url
  p $ "Text  : " <> cmt.body
  p $ T.pack ("Comments seen so far: " <> show count)

  let triggerText = "Haskell is great!!!!!!!!"
  let replyText = "Indeed, it is!"
  -- Check whether it contains the trigger text, and reply if necessary
  when
    (triggerText `T.isInfixOf` cmt.body)
    (p "Replying to it..." >> addNewComment cmt.id' replyText)
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
  -- The next line can be shortened with RecordWildCards if you want.
  let creds =
        Credentials
          { username = username,
            password = password,
            clientID = clientID,
            clientSecret = clientSecret
          }
  -- Get the RedditEnv term needed to run Reddit queries.
  env <- authenticate creds userAgent

  -- Run the bot, refreshing every 5 seconds (the default).
  runRedditT' env $ do
    commentStream replyIfHaskellGreat 1 "haskell"

  -- commentStream is a recursive function and, in principle, should never end.
  -- However, the implementation above is quite naive: in practice you have to
  -- account for exceptions, not least because Reddit servers seem to break
  -- about once every day. The easiest way to do this is to use the combinators
  -- in Control.Exception. The method shown here is quite crude, see e.g. the
  -- Control.Exception docs, or Neil Mitchell's blog post
  -- http://neilmitchell.blogspot.com/2015/05/handling-control-c-in-haskell.html
  -- for a better discussion.
  --
  -- There is a related issue, in that you can't extract the value of `count` so
  -- far if an exception is raised. If you wanted to do this properly, you
  -- probably need to use something like a `Data.IORef` to store the value of
  -- `count`.
  --
  -- Finally, note that, even though this nominally catches Ctrl-C exceptions,
  -- you can still kill the bot by pressing Ctrl-C twice:
  -- https://stackoverflow.com/questions/2349233
  let protectedAction =
        catch
          -- The original action
          (runRedditT' env $ commentStream replyIfHaskellGreat 1 "haskell")
          -- If we get an exception, print it, wait 5 seconds, then try again.
          ( \(e :: SomeException) -> do
              T.hPutStrLn stderr ("Exception: " <> T.pack (show e))
              threadDelay 5000000
              protectedAction
          )
  protectedAction
