{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- | Note that these types are not complete, in that we discard many fields of
-- the JSON response that Reddit gives. Making these fields exhaustive is the
-- subject of future work.
module Reddit.Types
  ( Comment (..),
    CommentListing (comments),
    Post (..),
    PostListing (posts),
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser (..))
import Data.Text (Text)
import qualified Data.Text as T

redditURL :: Text
redditURL = "https://reddit.com"

-- | Reddit \'things\' have an ID, which is a string prefixed by "t1_", "t2_", ...
-- Here, we parse them to remove the prefix.
data ID
  = CommentID Text -- t1_...
  | AccountID Text -- t2_...
  | PostID Text -- t3_...
  | MessageID Text -- t4_...
  | SubredditID Text -- t5_...
  | AwardID Text -- t6_...
  deriving (Eq, Ord, Show)

instance FromJSON ID where
  parseJSON = withText "ID" $ \t ->
    case T.splitAt 3 t of
      ("t1_", rest) -> pure (CommentID rest)
      ("t2_", rest) -> pure (AccountID rest)
      ("t3_", rest) -> pure (PostID rest)
      ("t4_", rest) -> pure (MessageID rest)
      ("t5_", rest) -> pure (SubredditID rest)
      ("t6_", rest) -> pure (AwardID rest)
      _ -> fail . T.unpack $ ("Failed to parse ID " <> t)

mkFullName :: ID -> Text
mkFullName (CommentID t) = "t1_" <> t
mkFullName (AccountID t) = "t2_" <> t
mkFullName (PostID t) = "t3_" <> t
mkFullName (MessageID t) = "t4_" <> t
mkFullName (SubredditID t) = "t5_" <> t
mkFullName (AwardID t) = "t6_" <> t

-- | A single comment.
data Comment = Comment
  { id' :: ID,
    url :: Text,
    author :: Text,
    body :: Text,
    subreddit :: Text,
    subreddit_id :: ID,
    post :: ID,
    parent :: ID
  }
  deriving (Eq, Ord, Show)

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \o -> do
    v <- o .: "data"
    id' <- CommentID <$> v .: "id"
    url <- (redditURL <>) <$> v .: "permalink"
    author <- v .: "author"
    body <- v .: "body"
    subreddit <- v .: "subreddit"
    subreddit_id <- v .: "subreddit_id"
    post <- v .: "link_id"
    parent <- v .: "parent_id"
    pure $ Comment {..}

-- | A single post
data Post = Post
  { id' :: ID,
    url :: Text,
    author :: Text,
    title :: Text,
    -- TODO: This only works for selftext posts
    body :: Text,
    subreddit :: Text,
    subreddit_id :: ID
  }
  deriving (Eq, Ord, Show)

instance FromJSON Post where
  parseJSON = withObject "Post" $ \o -> do
    v <- o .: "data"
    id' <- PostID <$> v .: "id"
    url <- (redditURL <>) <$> v .: "permalink"
    author <- v .: "author"
    title <- v .: "title"
    body <- v .: "selftext"
    subreddit <- v .: "subreddit"
    subreddit_id <- v .: "subreddit_id"
    pure $ Post {..}

-- | Comment listings
data CommentListing = CommentListing
  { after :: Text,
    size :: Int,
    comments :: [Comment]
  }
  deriving (Show)

instance FromJSON CommentListing where
  parseJSON = withObject "CommentListing" $ \o -> do
    v <- o .: "data"
    after <- v .: "after"
    size <- v .: "dist"
    comments <- v .: "children" >>= parseJSONList
    pure $ CommentListing {..}

-- | Post listings
data PostListing = PostListing
  { after :: Text,
    size :: Int,
    posts :: [Post]
  }
  deriving (Show)

instance FromJSON PostListing where
  parseJSON = withObject "PostListing" $ \o -> do
    v <- o .: "data"
    after <- v .: "after"
    size <- v .: "dist"
    posts <- v .: "children" >>= parseJSONList
    pure $ PostListing {..}
