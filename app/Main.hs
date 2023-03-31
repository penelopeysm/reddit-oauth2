{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Reddit
import System.Environment (getEnv)

getEnvAsText :: Text -> IO Text
getEnvAsText = fmap T.pack . getEnv . T.unpack

main :: IO ()
main = do
  username <- getEnvAsText "REDDIT_USERNAME"
  password <- getEnvAsText "REDDIT_PASSWORD"
  clientID <- getEnvAsText "REDDIT_ID"
  clientSecret <- getEnvAsText "REDDIT_SECRET"
  let userAgent = "github:penelopeysm/reddit-oauth2 by /u/is_a_togekiss"
  let creds = Credentials {..}

  env <- withCredentials creds userAgent

  let showC :: Int -> Comment -> RedditT Int
      showC count cmt = do
        let p = liftIO . T.putStrLn
        p ""
        p "Found new comment!"
        p $ "By    : /u/" <> cmt.author
        p $ "Link  : " <> cmt.url
        p $ "Text  : " <> cmt.body
        p $ "Posted at  : " <> T.pack (show cmt.created)
        pure (count + 1)

  runRedditT' env $ do
    p <- getComment (CommentID "jeccwt1")
    liftIO $ print p
    -- stream True showC 0 (subredditComments "AskReddit")
