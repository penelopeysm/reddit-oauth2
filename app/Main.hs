{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import Reddit
import System.Environment (getEnv)

getEnvAsText :: Text -> IO Text
getEnvAsText = fmap T.pack . getEnv . T.unpack

main :: IO ()
main = do
  redditUser <- getEnvAsText "REDDIT_USERNAME"
  redditPassword <- getEnvAsText "REDDIT_PASSWORD"
  redditID <- getEnvAsText "REDDIT_ID"
  redditSecret <- getEnvAsText "REDDIT_SECRET"
  let userAgent = "github:penelopeysm/reddit-oauth2 by /u/is_a_togekiss"
  let creds = Credentials {username = redditUser, password = redditPassword, clientID = redditID, clientSecret = redditSecret}

  env <- Reddit.authWithCredentials creds userAgent

  flip runReaderT env $ do
    -- resp1 <- Reddit.user "is_a_togekiss"
    -- liftIO $ B.putStrLn resp1

    resp2 <- Reddit.subComments "pokemontrades"
    liftIO $ B.putStrLn resp2
