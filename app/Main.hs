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
import qualified Reddit.Auth as Auth
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
  let creds = Auth.Credentials {..}

  env <- Auth.withCredentials creds userAgent

  let callback_with_state :: Int -> Comment -> IO Int
      callback_with_state count cmt = do
        T.putStrLn ""
        T.putStrLn ""
        T.putStrLn "Found new comment!"
        T.putStrLn $ "By    : /u/" <> cmt.author
        T.putStrLn $ "Link  : " <> cmt.url
        T.putStrLn $ "Text  : " <> cmt.body
        T.putStrLn $ T.pack ("Comments seen so far: " <> show (count + 1))
        pure (count + 1)

  flip runReaderT env $ do
    -- cmts <- subredditComments "pokemontrades"
    -- liftIO $ print cmts

    stream True (subredditComments "pokemontrades") callback_with_state 0
