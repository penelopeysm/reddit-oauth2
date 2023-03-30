{-# LANGUAGE OverloadedStrings #-}

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
  -- Read in your secrets from environment variables (or wherever you like).
  username <- getEnvAsText "REDDIT_USERNAME"
  password <- getEnvAsText "REDDIT_PASSWORD"
  clientID <- getEnvAsText "REDDIT_ID"
  clientSecret <- getEnvAsText "REDDIT_SECRET"
  let userAgent = "your user-agent here"
  let creds = Auth.Credentials {username = username,
      password = password,
      clientID = clientID,
      clientSecret = clientSecret}
  env <- Auth.withCredentials creds userAgent

  let replyToGreatHaskell :: Int -> Comment -> RedditT Int
      replyToGreatHaskell count cmt = do
        let p = liftIO . T.putStrLn
        p ""
        p "Found new comment!"
        p $ "By    : /u/" <> author cmt
        p $ "Link  : " <> url cmt
        p $ "Text  : " <> body cmt
        p $ T.pack ("Comments seen so far: " <> show (count + 1))

        let triggerText = "Haskell is great!"
        let replyText = "Indeed, it is!"
        when (triggerText `T.isInfixOf` (body cmt))
         (p "Replying to it..." >> addNewComment (id' cmt) replyText)
        pure (count + 1)

  runRedditT' env $ do
    stream True replyToGreatHaskell 0 (subredditComments "haskell")
