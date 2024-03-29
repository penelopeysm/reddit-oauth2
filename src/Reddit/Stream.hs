module Reddit.Stream
  ( StreamSettings (..),
    defaultStreamSettings,
    stream,
    stream',
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Exception (catch)
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ask)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Float.RealFracMethods (floorDoubleInt)
import qualified Reddit.Queue as Q
import Reddit.Types
import System.IO (hPrint, stderr)

-- | Available configuration variables for streams.
data StreamSettings = StreamSettings
  { -- | Delay (in seconds) between successive requests when using
    -- [streams](#streams). Reddit says you should not be querying more than 60
    -- times in a minute, so this should not go below 1. Defaults to 5.
    streamsDelay :: Double,
    -- | Number of \'seen\' items to keep in memory when running a stream. If
    -- you are requesting N items at a go, there doesn't appear to be much point
    -- in making this larger than N. Defaults to 250.
    streamsStorageSize :: Int,
    -- | Whether to catch errors in the stream. If set to @True@ (the default),
    -- then if a 'RedditException' is raised at any point in the stream, it is
    -- logged to stderr and the stream continues. If @False@, errors are
    -- propagated upwards.
    --
    -- Note that the Reddit API tends to break down quite regularly, so if you
    -- turn this off, chances are your stream will stop.
    streamsCatch :: Bool,
    -- | Whether to perform internal checks for item uniqueness. If set to
    -- @True@ (the default), then the Eq constraint is used to ensure that only
    -- unique items are passed to the callback function.
    --
    -- If @False@, then all items found are passed to the callback function.
    -- This is useful if you plan to do some other kind of uniqueness check,
    -- e.g. by looking it up in a database.
    streamsCheckUniqueness :: Bool
  }

-- | Default stream settings. See 'StreamSettings' for the specification of
-- these values.
defaultStreamSettings :: StreamSettings
defaultStreamSettings =
  StreamSettings
    { streamsDelay = 5,
      streamsStorageSize = 250,
      streamsCatch = True,
      streamsCheckUniqueness = True
    }

-- Helper function.
streamInner ::
  (Eq a, MonadIO m) =>
  StreamSettings ->
  Q.Queue a ->
  (t -> a -> RedditT m t) ->
  t ->
  (m [a] -> IO [a]) ->
  RedditT m [a] ->
  RedditT m ()
streamInner settings seen cb cbInit unwrapM src = do
  env <- ask
  let timer = threadDelay (floorDoubleInt (streamsDelay settings * 1000000))
  let getItems =
        if streamsCatch settings
          then do
            catch
              (unwrapM $ runRedditT env src)
              (\e -> hPrint stderr (e :: RedditException) >> pure [])
          else do
            unwrapM $ runRedditT env src
  (_, items) <- liftIO $ concurrently timer getItems
  let (seen', unique) = Q.merge items seen
  cbUpdated <- foldM cb cbInit $ if streamsCheckUniqueness settings then unique else items
  streamInner settings seen' cb cbUpdated unwrapM src

-- | If you have an action which generates a list of things (with the type
-- @RedditT [a]@), then "stream" turns this an action which
-- executes a callback function on an infinite list of things. It does so by
-- repeatedly fetching the list of either comments or posts.
--
-- Apart from the thing being acted on, the callback function is allowed to also
-- take, as input, some kind of state, and update that state by returning a new
-- value. This allows the user to, for example, keep track of how many things
-- have been seen so far, or perform actions conditionally based on what the
-- stream has previously thrown up.
--
-- You can adjust the frequency with which the stream is refreshed, and
-- secondly, the number of \'seen\' items which are stored in memory. These can
-- be changed using the 'streamsDelay' and 'streamsStorageSize' fields in the
-- 'StreamSettings' used.
stream ::
  (Eq a, MonadIO m) =>
  -- | Settings.
  StreamSettings ->
  -- | A callback function to execute on all things found.
  (state -> a -> RedditT m state) ->
  -- | The initial state for the callback function.
  state ->
  -- | A function which converts an action @MonadIO m => m [a]@ to a plain @IO
  -- [a]@. If you are running in the IO monad (i.e. @RedditT IO a@), then this
  -- can just be 'id'.
  (m [a] -> IO [a]) ->
  -- | The source of things to iterate over.
  RedditT m [a] ->
  RedditT m ()
stream settings cb cbInit unwrapM src = do
  first <- src
  streamInner settings (Q.fromList (streamsStorageSize settings) first) cb cbInit unwrapM src

-- | @stream'@ is a simpler version of @stream@, which accepts a callback that
-- doesn't use state.
stream' ::
  (Eq a, MonadIO m) =>
  -- | Settings.
  StreamSettings ->
  -- | A simple callback function which iterates over all things seen.
  (a -> RedditT m ()) ->
  -- | A function which converts an action @MonadIO m => m [a]@ to a plain @IO [a]@.
  (m [a] -> IO [a]) ->
  -- | The source of things to iterate over.
  RedditT m [a] ->
  RedditT m ()
stream' settings cb' = stream settings (const cb') ()
