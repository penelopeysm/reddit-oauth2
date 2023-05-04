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
  ( HiddenText (..),
    ID (..),
    Listing (..),
    Comment (..),
    CommentTree (..),
    Account (..),
    Post (..),
    Message (..),
    Subreddit (..),
    Award (..),
    CanCommentOn (..),
    HasID (..),
    EditedUTCTime (..),
    MoreChildren (..),
    addChildrenToTree,
    getFirstMore,
    removeFromTrees,
    treeToList,
    treeSize,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser (..))
import Data.List (foldl')
import Data.Monoid (First (..), getFirst)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- | Same as @Data.Text.Text@, but has a Show instance where it's not printed.
newtype HiddenText = HiddenText {getHiddenText :: Text} deriving (Eq, Ord)

instance Show HiddenText where
  show (HiddenText _) = "<redacted>"

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
  CommentID :: {unCommentID :: Text} -> ID Comment
  -- | t2_
  AccountID :: {unAccountID :: Text} -> ID Account
  -- | t3_
  PostID :: {unPostID :: Text} -> ID Post
  -- | t4_
  MessageID :: {unMessageID :: Text} -> ID Message
  -- | t5_
  SubredditID :: {unSubredditID :: Text} -> ID Subreddit
  -- | t6_
  AwardID :: {unAwardID :: Text} -> ID Award

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
    -- | The number of things contained in the listing. Sometimes this is null.
    size :: Maybe Int,
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
--
-- Note that the @Eq@ and @Ord@ instances for @Comment@ only compare the comment ID.
data Comment = Comment
  { id' :: ID Comment,
    -- | Permalink.
    url :: Text,
    score :: Int,
    -- | Username of the author (without the @\/u\/@).
    author :: Text,
    -- | Can be @Nothing@ if the account was deleted.
    author_id :: Maybe (ID Account),
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
  deriving (Show)

instance Eq Comment where
  c1 == c2 = c1.id' == c2.id'

instance Ord Comment where
  compare c1 c2 = compare c1.id' c2.id'

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \o -> do
    v <- o .: "data"
    id' <- CommentID <$> v .: "id"
    url <- (redditURL <>) <$> v .: "permalink"
    score <- v .: "score"
    author <- v .: "author"
    author_id <- v .:? "author_fullname"
    body <- v .: "body"
    subreddit <- v .: "subreddit"
    subreddit_id <- v .: "subreddit_id"
    post_id <- v .: "link_id"
    parent_id <- (Left <$> v .: "parent_id") <|> (Right <$> v .: "parent_id")
    is_submitter <- v .: "is_submitter"
    createdTime <- posixSecondsToUTCTime <$> v .: "created_utc"
    editedTime <- convertEditedTime <$> v .: "edited"
    pure $ Comment {..}

-- | When fetching comments on a specific post, we need a tree-like structure to
-- account for the relationship between comments.
--
-- Each post is associated with a list of top-level @CommentTree@s. Each
-- @CommentTree@ is either a comment together with its replies, or a special
-- @MoreComments@ value which represents the fact that a discussion continues
-- further, but the data were not returned immediately by Reddit.
data CommentTree
  = ActualComment Comment [CommentTree]
  | MoreComments [ID Comment]
  deriving (Eq, Show)

-- | Get all actual comments from inside a tree, ignoring all @MoreComments@ links.
treeToList :: CommentTree -> [Comment]
treeToList (MoreComments _) = []
treeToList (ActualComment c ts) = c : concatMap treeToList ts

-- | Number of actual comments contained within a tree.
treeSize :: CommentTree -> Int
treeSize = length . treeToList

instance FromJSON CommentTree where
  parseJSON = withObject "CommentTree" $ \o -> do
    (kind :: Text) <- o .: "kind"
    case kind of
      "more" -> do
        ids <- o .: "data" >>= (.: "children")
        pure $ MoreComments (map CommentID ids)
      "t1" -> do
        comment <- parseJSON (Object o)
        replies <- o .: "data" >>= (.: "replies")
        case replies of
          String "" -> pure $ ActualComment comment []
          v' -> do
            replies' <- contents <$> parseJSON v'
            pure $ ActualComment comment replies'
      s -> fail . T.unpack $ "Expected 'more' or 't1' in Listing, got " <> s

-- | Internal type to handle the return type of the api/morechildren endpoint.
--
-- Note that this endpoint does not return comments in a tree form, so, for
-- example, the @replies@ field of a @ReturnedCmt Comment@ will always be empty.
-- It is apparently left to the user to pick up the pieces and reconstruct the
-- tree, using the @parent_id@ field.
data MoreChildren
  = ReturnedCmt Comment
  | ReturnedMore (Either (ID Comment) (ID Post)) [ID Comment]
  deriving (Show)

instance FromJSON MoreChildren where
  parseJSON = withObject "api/morechildren" $ \o -> do
    (kind :: Text) <- o .: "kind"
    case kind of
      "more" -> do
        v <- o .: "data"
        ids <- v .: "children"
        parent_id <- (Left <$> v .: "parent_id") <|> (Right <$> v .: "parent_id")
        pure $ ReturnedMore parent_id (map CommentID ids)
      "t1" -> ReturnedCmt <$> parseJSON (Object o)
      s -> fail . T.unpack $ "Expected 'more' or 't1' in morechildren, got " <> s

addChildrenToTree :: [MoreChildren] -> [CommentTree] -> [CommentTree]
addChildrenToTree children tree = foldl' (flip updateTree) tree children
  where
    convert :: MoreChildren -> CommentTree
    convert (ReturnedCmt c) = ActualComment c []
    convert (ReturnedMore _ ids) = MoreComments ids
    -- The Bool in the return type tells us whether it was added to the tree or
    -- not.
    addToOneTree :: MoreChildren -> CommentTree -> (Bool, CommentTree)
    addToOneTree _ m@(MoreComments _) = (False, m)
    addToOneTree child (ActualComment c replies) =
      let parent_id = case child of
            ReturnedCmt c -> c.parent_id
            ReturnedMore p _ -> p
       in if Left c.id' == parent_id
            then (True, ActualComment c (replies ++ [convert child]))
            else
              let results = map (addToOneTree child) replies
                  found = any fst results
                  replies' = map snd results
               in (found, ActualComment c replies')
    updateTree :: MoreChildren -> [CommentTree] -> [CommentTree]
    updateTree child trees =
      let parent_id = case child of
            ReturnedCmt c -> c.parent_id
            ReturnedMore p _ -> p
       in case parent_id of
            Left (CommentID _) -> case trees of
              [] -> []
              (t : ts) ->
                let (added, t') = addToOneTree child t
                 in if added
                      then t' : ts
                      else t : updateTree child ts
            Right (PostID _) -> trees ++ [convert child]

-- | Find the first instance of @MoreComments@ in a tree.
getFirstMore :: [CommentTree] -> Maybe CommentTree
getFirstMore trees =
  -- Find in a single tree
  let getFirstMore' :: CommentTree -> Maybe CommentTree
      getFirstMore' m@(MoreComments ids) = Just m
      getFirstMore' (ActualComment _ replies) = getFirstMore replies
   in getFirst . mconcat . map (First . getFirstMore') $ trees

-- | Remove a branch from a tree
removeFromTrees :: CommentTree -> [CommentTree] -> [CommentTree]
removeFromTrees _ [] = []
removeFromTrees c (t : ts) = case rmv c t of
  (Nothing, _) -> ts -- t itself was the thing to be removed
  (Just t', True) -> t' : ts
  (Just t', False) -> t' : removeFromTrees c ts
  where
    -- Remove from a single tree. Bool indicates whether it was removed.
    rmv :: CommentTree -> CommentTree -> (Maybe CommentTree, Bool)
    rmv c t =
      if c == t
        then (Nothing, True)
        else case t of
          ActualComment cmt replies ->
            let replies' = removeFromTrees c replies
                removed = length replies /= length replies'
             in (Just $ ActualComment cmt replies', removed)
          m -> (Just m, False)

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

-- | A single post.
--
-- Note that the @Eq@ and @Ord@ instances for @Post@ only compare the post ID.
data Post = Post
  { id' :: ID Post,
    -- | Permalink.
    url :: Text,
    score :: Int,
    -- | Username of the author (without the @\/u\/@).
    author :: Text,
    -- | Can be @Nothing@ if the account was deleted.
    author_id :: Maybe (ID Account),
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
  deriving (Show)

instance Eq Post where
  p1 == p2 = p1.id' == p2.id'

instance Ord Post where
  compare p1 p2 = compare p1.id' p2.id'

instance FromJSON Post where
  parseJSON = withObject "Post" $ \o -> do
    v <- o .: "data"
    id' <- PostID <$> v .: "id"
    url <- (redditURL <>) <$> v .: "permalink"
    score <- v .: "score"
    author <- v .: "author"
    author_id <- v .:? "author_fullname"
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
