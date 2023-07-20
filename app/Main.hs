{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Reddit
import System.Environment (getEnv)
import System.IO

getEnvAsText :: Text -> IO Text
getEnvAsText = fmap T.pack . getEnv . T.unpack

printTree :: Int -> CommentTree -> IO ()
printTree n (MoreComments chldn) =
  T.putStrLn $
    T.replicate n " "
      <> "More: "
      <> T.intercalate "," (map mkFullNameFromID chldn)
printTree n (ActualComment c rpls) = do
  T.putStrLn $ T.replicate n " " <> mkFullName c
  mapM_ (printTree (n + 2)) rpls

main :: IO ()
main = do
  ownerUsername <- getEnvAsText "REDDIT_USERNAME"
  ownerPassword <- getEnvAsText "REDDIT_PASSWORD"
  ownerClientId <- getEnvAsText "REDDIT_ID"
  ownerClientSecret <- getEnvAsText "REDDIT_SECRET"
  let userAgent = "github:penelopeysm/reddit-oauth2 by /u/is_a_togekiss"
  let creds = OwnerCredentials {..}

  env <- authenticate creds userAgent
  hSetBuffering stdout NoBuffering

  runRedditT' env $ do
    post <- getPost (PostID "1555erh")
    liftIO $ print post
