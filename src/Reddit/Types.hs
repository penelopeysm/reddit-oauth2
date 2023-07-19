{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      : Reddit.Types
-- Description : Underlying types for the things returned by Reddit
-- Copyright   : (c) Penelope Yong 2023
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
import qualified Data.Set as S
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
  mkFullName cmt = mkFullNameFromID (commentId cmt)
  mkFullNameFromID (CommentID c_id) = "t1_" <> c_id

instance HasID Account where
  mkFullName acc = mkFullNameFromID (accountId acc)
  mkFullNameFromID (AccountID a_id) = "t2_" <> a_id

instance HasID Post where
  mkFullName pst = mkFullNameFromID (postId pst)
  mkFullNameFromID (PostID p_id) = "t3_" <> p_id

instance HasID Message where
  mkFullName msg = mkFullNameFromID (messageId msg)
  mkFullNameFromID (MessageID m_id) = "t4_" <> m_id

instance HasID Subreddit where
  mkFullName srd = mkFullNameFromID (subredditId srd)
  mkFullNameFromID (SubredditID s_id) = "t5_" <> s_id

instance HasID Award where
  mkFullName awd = mkFullNameFromID (awardId awd)
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
  { commentId :: ID Comment,
    -- | Permalink.
    commentUrl :: Text,
    commentScore :: Int,
    -- | Username of the author (without the @\/u\/@).
    commentAuthor :: Text,
    -- | Can be @Nothing@ if the account was deleted.
    commentAuthorId :: Maybe (ID Account),
    -- | Body text in Markdown.
    commentBody :: Text,
    -- | Name of the subreddit it was posted on (without the @\/r\/@ prefix).
    commentSubreddit :: Text,
    commentSubredditId :: ID Subreddit,
    commentPostId :: ID Post,
    -- | Whether the commenter also made the post.
    commentIsSubmitter :: Bool,
    -- | If the comment is a top-level comment, this is the same as @Right@ of @post_id@; otherwise it's @Left@ of parent comment's ID.
    commentParentId :: Either (ID Comment) (ID Post),
    commentCreatedTime :: UTCTime,
    commentEditedTime :: EditedUTCTime
  }
  deriving (Show)

instance Eq Comment where
  c1 == c2 = commentId c1 == commentId c2

instance Ord Comment where
  compare c1 c2 = compare (commentId c1) (commentId c2)

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \o -> do
    v <- o .: "data"
    commentId <- CommentID <$> v .: "id"
    commentUrl <- (redditURL <>) <$> v .: "permalink"
    commentScore <- v .: "score"
    commentAuthor <- v .: "author"
    commentAuthorId <- v .:? "author_fullname"
    commentBody <- v .: "body"
    commentSubreddit <- v .: "subreddit"
    commentSubredditId <- v .: "subreddit_id"
    commentPostId <- v .: "link_id"
    commentParentId <- (Left <$> v .: "parent_id") <|> (Right <$> v .: "parent_id")
    commentIsSubmitter <- v .: "is_submitter"
    commentCreatedTime <- posixSecondsToUTCTime <$> v .: "created_utc"
    commentEditedTime <- convertEditedTime <$> v .: "edited"
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
            ReturnedCmt c -> commentParentId c
            ReturnedMore p _ -> p
       in if Left (commentId c) == parent_id
            then (True, ActualComment c (replies ++ [convert child]))
            else
              let results = map (addToOneTree child) replies
                  found = any fst results
                  replies' = map snd results
               in (found, ActualComment c replies')
    updateTree :: MoreChildren -> [CommentTree] -> [CommentTree]
    updateTree child trees =
      let parent_id = case child of
            ReturnedCmt c -> commentParentId c
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

-- | A Reddit account.
data Account = Account
  { accountId :: ID Account,
    -- | Username (without the @\/u\/@ prefix).
    accountUsername :: Text,
    -- | The name that's shown on their profile. Empty if not set.
    accountDisplayName :: Text,
    accountLinkKarma :: Int,
    accountCommentKarma :: Int,
    accountTotalKarma :: Int,
    accountCreatedTime :: UTCTime
  }
  deriving (Eq, Ord, Show)

instance FromJSON Account where
  parseJSON = withObject "Account" $ \o -> do
    v <- o .: "data"
    accountId <- AccountID <$> v .: "id"
    accountUsername <- v .: "name"
    accountDisplayName <- v .: "subreddit" >>= (.: "title")
    accountLinkKarma <- v .: "link_karma"
    accountCommentKarma <- v .: "comment_karma"
    accountTotalKarma <- v .: "total_karma"
    accountCreatedTime <- posixSecondsToUTCTime <$> v .: "created_utc"
    pure $ Account {..}

-- Post

-- | A single post.
--
-- Note that the @Eq@ and @Ord@ instances for @Post@ only compare the post ID.
data Post = Post
  { postId :: ID Post,
    -- | Permalink.
    postUrl :: Text,
    postScore :: Int,
    -- | Number of upvotes. Note that this is fuzzed by Reddit.
    postUpvotes :: Int,
    -- | Upvote to downvote ratio. Note that this is fuzzed by Reddit.
    postUpvoteRatio :: Double,
    -- | Username of the author (without the @\/u\/@).
    postAuthor :: Text,
    -- | Can be @Nothing@ if the account was deleted.
    postAuthorId :: Maybe (ID Account),
    postTitle :: Text,
    -- | Empty string if not a text post.
    postBody :: Text,
    -- | HTML version of post body. Empty string if not a text post.
    postBodyHtml :: Text,
    -- | For a link post, this is the link. For a text post, this is the same as @url@.
    postContentUrl :: Text,
    -- | None if not flaired.
    postFlairText :: Maybe Text,
    -- | Name of the subreddit (without the @\/r\/@)
    postSubreddit :: Text,
    postSubredditId :: ID Subreddit,
    postCreatedTime :: UTCTime,
    postEditedTime :: EditedUTCTime,
    -- | Whether the logged in user saved the post.
    postSaved :: Bool,
    -- | Whether the logged in user hid the post.
    postHidden :: Bool,
    postLocked :: Bool,
    postStickied :: Bool
  }
  deriving (Show)

instance Eq Post where
  p1 == p2 = postId p1 == postId p2

instance Ord Post where
  compare p1 p2 = compare (postId p1) (postId p2)

instance FromJSON Post where
  parseJSON = withObject "Post" $ \o -> do
    v <- o .: "data"
    postId <- PostID <$> v .: "id"
    postUrl <- (redditURL <>) <$> v .: "permalink"
    postScore <- v .: "score"
    postUpvotes <- v .: "ups"
    postUpvoteRatio <- v .: "upvote_ratio"
    postAuthor <- v .: "author"
    postAuthorId <- v .:? "author_fullname"
    postTitle <- v .: "title"
    postBody <- v .: "selftext"
    postBodyHtml <- v .: "selftext_html"
    postContentUrl <- v .: "url"
    postFlairText <- v .: "link_flair_text"
    postSubreddit <- v .: "subreddit"
    postSubredditId <- v .: "subreddit_id"
    postCreatedTime <- posixSecondsToUTCTime <$> v .: "created_utc"
    postEditedTime <- convertEditedTime <$> v .: "edited"
    postSaved <- v .: "saved"
    postHidden <- v .: "hidden"
    postLocked <- v .: "locked"
    postStickied <- v .: "stickied"
    pure $ Post {..}

-- Subreddit

-- | A subreddit.
data Subreddit = Subreddit
  { subredditId :: ID Subreddit,
    -- | Subreddit name (without the @\/r\/@).
    subredditName :: Text,
    -- | Human name (the string that appears at the top of the sub).
    subredditTitle :: Text,
    -- | Sidebar text.
    subredditDescription :: Text,
    -- | Number of subscribers.
    subredditSubscribers :: Int,
    -- | Created time.
    subredditCreated :: UTCTime
  }
  deriving (Eq, Ord, Show)

instance FromJSON Subreddit where
  parseJSON = withObject "Subreddit" $ \o -> do
    v <- o .: "data"
    subredditId <- SubredditID <$> v .: "id"
    subredditName <- v .: "display_name"
    subredditDescription <- v .: "public_description"
    subredditTitle <- v .: "title"
    subredditSubscribers <- v .: "subscribers"
    subredditCreated <- posixSecondsToUTCTime <$> v .: "created"
    pure $ Subreddit {..}

-- Award

-- | A Reddit award. Not implemented yet.
data Award = Award
  {awardId :: ID Award}
  deriving (Eq, Ord, Show)

-- Message

-- | A Reddit message (these refer to direct messages, not chat). Not implemented yet.
data Message = Message
  {messageId :: ID Message}
  deriving (Eq, Ord, Show)
