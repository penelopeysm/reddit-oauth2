{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      : Reddit.Types
-- Description : Underlying types for the things returned by Reddit
-- Copyright   : (c) Penelope Y. 2023
-- License     : MIT
-- Maintainer  : penelopeysm@gmail.com
-- Stability   : experimental
--
-- Note that these types are not complete, in that we discard many fields of
-- the JSON response that Reddit gives. If there is a particular piece of
-- information you need, please feel free to make an issue or PR.
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
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

redditURL :: Text
redditURL = "https://reddit.com"

-- | Reddit \'things\' have an ID, which (in the raw JSON) is a string prefixed
-- by "t1_", "t2_", "t3_"... depending on what type of object it is. In this
-- library, IDs are stored __without__ this prefix; so @CommentID "abcde"@ is
-- equivalent to Reddit's @"t1_abcde"@.
--
-- By the way, this is my first time dabbling with GADTs in a library, so I'm
-- not completely sure if it's the correct (or the smartest) way to do it.
-- Please get in touch if you want to tell me about alternatives etc.
data ID a where
  -- | t1_
  CommentID :: Text -> ID Comment
  -- | t2_
  AccountID :: Text -> ID Account
  -- | t3_
  PostID :: Text -> ID Post
  -- | t4_
  MessageID :: Text -> ID Message
  -- | t5_
  SubredditID :: Text -> ID Subreddit
  -- | t6_
  AwardID :: Text -> ID Award

deriving instance (Show a) => Show (ID a)

deriving instance (Eq a) => Eq (ID a)

deriving instance (Ord a) => Ord (ID a)

-- | Things which have an ID.
class HasID a where
  -- | Construct the full name of a thing from the thing itself.
  mkFullName :: a -> Text

  -- | Construct the full name of a thing from its ID. This is possible because
  -- the ID itself carries information about its type, allowing us to figure out
  -- the correct prefix.
  mkFullNameFromID :: ID a -> Text

instance HasID Comment where
  mkFullName cmt = mkFullNameFromID cmt.id'
  mkFullNameFromID (CommentID c_id) = "t1_" <> c_id

instance HasID Account where
  mkFullName acc = mkFullNameFromID acc.id'
  mkFullNameFromID (AccountID a_id) = "t2_" <> a_id

instance HasID Post where
  mkFullName pst = mkFullNameFromID pst.id'
  mkFullNameFromID (PostID p_id) = "t3_" <> p_id

instance HasID Message where
  mkFullName msg = mkFullNameFromID msg.id'
  mkFullNameFromID (MessageID m_id) = "t4_" <> m_id

instance HasID Subreddit where
  mkFullName srd = mkFullNameFromID srd.id'
  mkFullNameFromID (SubredditID s_id) = "t5_" <> s_id

instance HasID Award where
  mkFullName awd = mkFullNameFromID awd.id'
  mkFullNameFromID (AwardID a_id) = "t6_" <> a_id

-- | Things that can be commented on (i.e. comments and posts).
class (HasID a) => CanCommentOn a

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

-- Comment

-- | A single comment.
data Comment = Comment
  { id' :: ID Comment,
    -- | Permalink
    url :: Text,
    score :: Int,
    -- | Username of author
    author :: Text,
    author_id :: ID Account,
    -- | Body text
    body :: Text,
    -- | Name of the subreddit it was posted on
    subreddit :: Text,
    subreddit_id :: ID Subreddit,
    post_id :: ID Post,
    -- | Whether the commenter also made the post
    is_submitter :: Bool,
    -- | If the comment is a top-level comment, this is the same as @post_id@; otherwise it's the ID of the parent comment.
    parent_id :: Either (ID Comment) (ID Post),
    created :: UTCTime
  }
  deriving (Eq, Ord, Show)

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \o -> do
    v <- o .: "data"
    id' <- CommentID <$> v .: "id"
    url <- (redditURL <>) <$> v .: "permalink"
    score <- v .: "score"
    author <- v .: "author"
    author_id <- v .: "author_fullname"
    body <- v .: "body"
    subreddit <- v .: "subreddit"
    subreddit_id <- v .: "subreddit_id"
    post_id <- v .: "link_id"
    parent_id <- (Left <$> v .: "parent_id") <|> (Right <$> v .: "parent_id")
    is_submitter <- v .: "is_submitter"
    created <- posixSecondsToUTCTime <$> v .: "created_utc"
    pure $ Comment {..}

-- | Comment listings
data CommentListing = CommentListing
  { -- | may be @None@ if there is nothing to come after it
    after :: Maybe Text,
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

-- Post

-- | A single post
data Post = Post
  { id' :: ID Post,
    -- | Permalink.
    url :: Text,
    score :: Int,
    -- | Username of the author.
    author :: Text,
    author_id :: ID Account,
    title :: Text,
    -- | Empty string if not a text post
    body :: Text,
    -- | For a link post, this is the link. For a text post, this is the same as @url@
    content_url :: Text,
    -- | None if not flaired.
    flairtext :: Maybe Text,
    subreddit :: Text,
    subreddit_id :: ID Subreddit,
    created :: UTCTime
  }
  deriving (Eq, Ord, Show)

instance FromJSON Post where
  parseJSON = withObject "Post" $ \o -> do
    v <- o .: "data"
    id' <- PostID <$> v .: "id"
    url <- (redditURL <>) <$> v .: "permalink"
    score <- v .: "score"
    author <- v .: "author"
    author_id <- v .: "author_fullname"
    title <- v .: "title"
    body <- v .: "selftext"
    content_url <- v .: "url"
    flairtext <- v .: "link_flair_text"
    subreddit <- v .: "subreddit"
    subreddit_id <- v .: "subreddit_id"
    created <- posixSecondsToUTCTime <$> v .: "created_utc"
    pure $ Post {..}

-- | Post listings
data PostListing = PostListing
  { -- | may be @None@ if there is nothing to come after it
    after :: Maybe Text,
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

-- Subreddit

data Subreddit = Subreddit
  {id' :: ID Subreddit}
  deriving (Eq, Ord, Show)

-- Account

data Account = Account
  { id' :: ID Account,
    created :: UTCTime
  }
  deriving (Eq, Ord, Show)

-- Award

data Award = Award
  {id' :: ID Award}
  deriving (Eq, Ord, Show)

data Message = Message
  {id' :: ID Message}
  deriving (Eq, Ord, Show)
