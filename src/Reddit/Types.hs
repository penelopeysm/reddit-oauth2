{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Note that these types are not complete, in that we discard many fields of
-- the JSON response that Reddit gives. Making these fields exhaustive is the
-- subject of future work.
module Reddit.Types
  ( ID (..),
    Comment (..),
    CommentListing (comments),
    Post (..),
    PostListing (posts),
    CanCommentOn (..),
    HasID (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser (..))
import Data.Text (Text)
import qualified Data.Text as T

redditURL :: Text
redditURL = "https://reddit.com"

-- | Reddit \'things\' have an ID, which is a string prefixed by "t1_", "t2_", ...
-- Here, we parse them to remove the prefix.
-- data ID = CommentID Text
--         | AccountID Text
--         | PostID Text
--         | MessageID Text
--         | SubredditID Text
--         | AwardID Text
--   deriving (Eq, Ord, Show)
data ID a where
  CommentID :: Text -> ID Comment      -- t1_
  AccountID :: Text -> ID Account      -- t2_
  PostID :: Text -> ID Post            -- t3_
  MessageID :: Text -> ID Message      -- t4_
  SubredditID :: Text -> ID Subreddit  -- t5_
  AwardID :: Text -> ID Award          -- t6_
deriving instance Show a => Show (ID a)
deriving instance Eq a => Eq (ID a)
deriving instance Ord a => Ord (ID a)

class HasID a where
  mkFullName :: a -> Text
  mkFullNameFromID :: ID a -> Text

instance HasID Comment where
  mkFullNameFromID (CommentID c_id) = "t1_" <> c_id
  mkFullName cmt = mkFullNameFromID cmt.id'

instance HasID Account where
  mkFullNameFromID (AccountID a_id) = "t2_" <> a_id
  mkFullName acc = mkFullNameFromID acc.id'

instance HasID Post where
  mkFullNameFromID (PostID p_id) = "t3_" <> p_id
  mkFullName pst = mkFullNameFromID pst.id'

instance HasID Message where
  mkFullNameFromID (MessageID m_id) = "t4_" <> m_id
  mkFullName msg = mkFullNameFromID msg.id'

instance HasID Subreddit where
  mkFullNameFromID (SubredditID s_id) = "t5_" <> s_id
  mkFullName srd = mkFullNameFromID srd.id'

instance HasID Award where
  mkFullNameFromID (AwardID a_id) = "t6_" <> a_id
  mkFullName awd = mkFullNameFromID awd.id'

class HasID a => CanCommentOn a where

instance CanCommentOn Comment

instance CanCommentOn Post

instance FromJSON (ID Comment) where
  parseJSON = withText "commentID" $ \t ->
    case T.splitAt 3 t of
      ("t1_", rest) -> pure (CommentID rest)
      _ -> fail . T.unpack $ ("Failed to parse ID " <> t)

instance FromJSON (ID Account) where
  parseJSON = withText "accountID" $ \t ->
    case T.splitAt 3 t of
      ("t2_", rest) -> pure (AccountID rest)
      _ -> fail . T.unpack $ ("Failed to parse ID " <> t)

instance FromJSON (ID Post) where
  parseJSON = withText "postID" $ \t ->
    case T.splitAt 3 t of
      ("t3_", rest) -> pure (PostID rest)
      _ -> fail . T.unpack $ ("Failed to parse ID " <> t)

instance FromJSON (ID Message) where
  parseJSON = withText "messageID" $ \t ->
    case T.splitAt 3 t of
      ("t4_", rest) -> pure (MessageID rest)
      _ -> fail . T.unpack $ ("Failed to parse ID " <> t)

instance FromJSON (ID Subreddit) where
  parseJSON = withText "postID" $ \t ->
    case T.splitAt 3 t of
      ("t5_", rest) -> pure (SubredditID rest)
      _ -> fail . T.unpack $ ("Failed to parse ID " <> t)

instance FromJSON (ID Award) where
  parseJSON = withText "awardID" $ \t ->
    case T.splitAt 3 t of
      ("t6_", rest) -> pure (AwardID rest)
      _ -> fail . T.unpack $ ("Failed to parse ID " <> t)

-- | A single comment.
data Comment = Comment
  { id' :: ID Comment,
    url :: Text,
    author :: Text,
    body :: Text,
    subreddit :: Text,
    subreddit_id :: ID Subreddit,
    post_id :: ID Post,
    parent_id :: Either (ID Comment) (ID Post)  -- | If the comment is a top-level post, this is the same as @post_id@; otherwise it's the ID of the parent comment.
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
    post_id <- v .: "link_id"
    parent_id <- (Left <$> v .: "parent_id") <|> (Right <$> v .: "parent_id")
    pure $ Comment {..}

-- | A single post
data Post = Post
  { id' :: ID Post,
    url :: Text,
    author :: Text,
    title :: Text,
    body :: Text,   -- | Empty if not a selftext post. TODO: sort out link posts
    subreddit :: Text,
    subreddit_id :: ID Subreddit
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
  { after :: Maybe Text, -- | may be @None@ if there is nothing to come after it
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
  { after :: Maybe Text, -- | may be @None@ if there is nothing to come after it
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

data Subreddit = Subreddit
               { id' :: ID Subreddit }
               deriving (Eq, Ord, Show)

data Account = Account
           { id' :: ID Account }
           deriving (Eq, Ord, Show)

data Award = Award
           { id' :: ID Award }
           deriving (Eq, Ord, Show)

data Message = Message
           { id' :: ID Message }
           deriving (Eq, Ord, Show)
