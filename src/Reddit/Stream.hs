module Reddit.Stream where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ask)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Float.RealFracMethods (floorDoubleInt)
import qualified Reddit.Queue as Q
import Reddit.Types

-- | Available configuration variables for streams.
data StreamSettings = StreamSettings
  { -- | Delay (in seconds) between successive requests when using
    -- [streams](#streams). Reddit says you should not be querying more than 60
    -- times in a minute, so this should not go below 1. Defaults to 5.
    streamsDelay :: Double,
    -- | Number of \'seen\' items to keep in memory when running a stream. If
    -- you are requesting N items at a go, there doesn't appear to be much point
    -- in making this larger than N. N should probably be 100, but this defaults
    -- to 250 to be safe, because I'm not sure if there are weird edge cases.
    streamsStorageSize :: Int
  }

-- | Default stream settings. See 'StreamSettings' for the specification of
-- these values.
defaultStreamSettings :: StreamSettings
defaultStreamSettings =
  StreamSettings
    { streamsDelay = 5,
      streamsStorageSize = 250
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
streamInner settings queue cb cbInit unwrapM src = do
  liftIO $ threadDelay (floorDoubleInt (streamsDelay settings * 1000000))
  env <- ask

  items <-
    liftIO $
      catch
        (unwrapM $ runRedditT env src)
        (\e -> print (e :: RedditException) >> pure [])
  -- `queue` essentially contains the last N items we've seen.
  items <- src
  let (queue', unique) = Q.merge items queue
  cbUpdated <- foldM cb cbInit unique
  streamInner settings queue' cb cbUpdated unwrapM src

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
