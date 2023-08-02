module Reddit.Exception (RedditException (..), throwIOJson, throwIOApi) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req (HttpException)

-- | The type of exceptions thrown by this library.
data RedditException
  = -- | Network request that failed.
    RedditReqException HttpException
  | -- | Invalid JSON returned by the Reddit API.
    RedditJsonException Text
  | -- | Some other kind of Reddit API error.
    RedditApiException Text
  deriving (Show)

instance Exception RedditException

throwIOJson :: (MonadIO m) => String -> m a
throwIOJson = liftIO . throwIO . RedditJsonException . T.pack

throwIOApi :: (MonadIO m) => String -> m a
throwIOApi = liftIO . throwIO . RedditApiException . T.pack
