{-# LANGUAGE DuplicateRecordFields #-}
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
    Listing (..),
    Comment (..),
    Account (..),
    Post (..),
    Message (..),
    Subreddit (..),
    Award (..),
    CanCommentOn (..),
    HasID (..),
    EditedUTCTime (..),
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
-- by @"t1_"@, @"t2_"@, @"t3_"@... depending on what type of object it is. In
-- this library, IDs are stored __without__ this prefix; so @CommentID "abcde"@
-- is equivalent to Reddit's @"t1_abcde"@.
--
-- By the way, this is my first time dabbling with GADTs in a library, so I'm
-- not completely sure if it's the correct (or the smartest) way to do it.
-- Please get in touch if you want to tell me about alternatives.
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

-- Listings

-- | @Listing@s are Reddit's way of paginating results. Reddit actually supports
-- heterogeneous listings (which can contain, say, comments, posts, and other
-- things), and this would probably improve performance for some applications,
-- but this isn't really something I'm keen on implementing right now.
data Listing t = Listing
  { -- | May be @None@ if there is nothing to come after it.
    after :: Maybe Text,
    -- | The number of things contained in the listing.
    size :: Int,
    -- | The things inside it.
    contents :: [t]
  }

instance (FromJSON t) => FromJSON (Listing t) where
  parseJSON = withObject "Listing" $ \o -> do
    v <- o .: "data"
    after <- v .: "after"
    size <- v .: "dist"
    contents <- v .: "children" >>= parseJSONList
    pure $ Listing {..}

-- Convenience functions to deal with some weird responses from Reddit's API

-- | Reddit's API does not always preserve the time that a comment was edited
-- (particularly for old comments, I think), so these represent the three
-- possibilities for the @editedTime@ field of comments and posts.
data EditedUTCTime
  = NeverEdited
  | EditedButTimeUnknown
  | EditedAt UTCTime
  deriving (Eq, Ord, Show)

-- | Convert Reddit's \'edited\' field (which is either a bool or a number...!)
-- to our "EditedUTCTime" type.
convertEditedTime :: Value -> EditedUTCTime
convertEditedTime (Bool b) = if b then EditedButTimeUnknown else NeverEdited
convertEditedTime (Number n) = EditedAt (posixSecondsToUTCTime . realToFrac $ n)

-- Comment

-- | A single comment.
data Comment = Comment
  { id' :: ID Comment,
    -- | Permalink.
    url :: Text,
    score :: Int,
    -- | Username of the author (without the @\/u\/@).
    author :: Text,
    author_id :: ID Account,
    -- | Body text in Markdown.
    body :: Text,
    -- | Name of the subreddit it was posted on (without the @\/r\/@ prefix).
    subreddit :: Text,
    subreddit_id :: ID Subreddit,
    post_id :: ID Post,
    -- | Whether the commenter also made the post.
    is_submitter :: Bool,
    -- | If the comment is a top-level comment, this is the same as @Right@ of @post_id@; otherwise it's @Left@ of parent comment's ID.
    parent_id :: Either (ID Comment) (ID Post),
    createdTime :: UTCTime,
    editedTime :: EditedUTCTime
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
    createdTime <- posixSecondsToUTCTime <$> v .: "created_utc"
    editedTime <- convertEditedTime <$> v .: "edited"
    pure $ Comment {..}

-- Account

data Account = Account
  { id' :: ID Account,
    -- | Username (without the @\/u\/@ prefix).
    username :: Text,
    -- | The name that's shown on their profile. Empty if not set.
    display_name :: Text,
    link_karma :: Int,
    comment_karma :: Int,
    total_karma :: Int,
    createdTime :: UTCTime
  }
  deriving (Eq, Ord, Show)

instance FromJSON Account where
  parseJSON = withObject "Account" $ \o -> do
    v <- o .: "data"
    id' <- AccountID <$> v .: "id"
    username <- v .: "name"
    display_name <- v .: "subreddit" >>= (.: "title")
    link_karma <- v .: "link_karma"
    comment_karma <- v .: "comment_karma"
    total_karma <- v .: "total_karma"
    createdTime <- posixSecondsToUTCTime <$> v .: "created_utc"
    pure $ Account {..}

-- Post

-- | A single post
data Post = Post
  { id' :: ID Post,
    -- | Permalink.
    url :: Text,
    score :: Int,
    -- | Username of the author (without the @\/u\/@).
    author :: Text,
    author_id :: ID Account,
    title :: Text,
    -- | Empty string if not a text post.
    body :: Text,
    -- | For a link post, this is the link. For a text post, this is the same as @url@.
    content_url :: Text,
    -- | None if not flaired.
    flairtext :: Maybe Text,
    -- | Name of the subreddit (without the @\/r\/@)
    subreddit :: Text,
    subreddit_id :: ID Subreddit,
    createdTime :: UTCTime,
    editedTime :: EditedUTCTime
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
    createdTime <- posixSecondsToUTCTime <$> v .: "created_utc"
    editedTime <- convertEditedTime <$> v .: "edited"
    pure $ Post {..}

-- Subreddit

data Subreddit = Subreddit
  { id' :: ID Subreddit,
    -- | Subreddit name (without the @\/r\/@).
    name :: Text,
    -- | Human name (the string that appears at the top of the sub).
    title :: Text,
    -- | Sidebar text.
    description :: Text,
    -- | Number of subscribers.
    subscribers :: Int,
    -- | Created time.
    created :: UTCTime
  }
  deriving (Eq, Ord, Show)

instance FromJSON Subreddit where
  parseJSON = withObject "Subreddit" $ \o -> do
    v <- o .: "data"
    id' <- SubredditID <$> v .: "id"
    name <- v .: "display_name"
    description <- v .: "public_description"
    title <- v .: "title"
    subscribers <- v .: "subscribers"
    created <- posixSecondsToUTCTime <$> v .: "created"
    pure $ Subreddit {..}

-- Award

data Award = Award
  {id' :: ID Award}
  deriving (Eq, Ord, Show)

data Message = Message
  {id' :: ID Message}
  deriving (Eq, Ord, Show)
