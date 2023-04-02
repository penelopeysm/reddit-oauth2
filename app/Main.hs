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
import System.IO (hFlush, stdout)

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

  defEnv <- withCredentials creds userAgent

  let env = defEnv {streamStorageSize = 150}

  let showCommentInfo :: Comment -> IO ()
      showCommentInfo c = do
        putStr (show c.createdTime)
        putStr " "
        putStr (show c.id')
        putStr " "
        putStr (show c.editedTime)
        putStr " /u/"
        T.putStrLn c.author
        hFlush stdout

  runRedditT' env $ do
    commentStream (const $ liftIO . showCommentInfo) () "askreddit"
    -- cmts <- subredditComments 250 "pokemontrades"
    -- liftIO $ mapM_ showCommentInfo cmts
