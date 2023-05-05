-- |
-- Module      : Reddit.Queue
-- Description : A bounded queue used for streams.
-- Copyright   : (c) Penelope Yong 2023
-- License     : MIT
-- Maintainer  : penelopeysm@gmail.com
-- Stability   : experimental
module Reddit.Queue (Queue (..), empty, fromList, merge) where

import Data.Foldable (elem)
import Data.List (foldl')

-- | Implementation of a bounded queue (i.e. a queue with a maximum size). When
-- the queue is full, adding new items to the back of the queue causes items to
-- be dequeued from the front.
--
-- The standard trick of using two lists, one for the \'front\' and one for the
-- \'back', is used here.
data Queue a = Queue
  { -- | Elements are removed from the beginning of this list
    front :: [a],
    -- | Elements are queued at the beginning of this list
    back :: [a],
    -- | Invariant: length front + length back == size and size <= maxSize
    size :: !Int,
    -- | Maximum size of the queue.
    maxSize :: Int
  }
  deriving (Show)

-- | Make a new, empty, queue with the specified maximum size.
empty :: Int -> Queue a
empty n =
  Queue
    { front = [],
      back = [],
      size = 0,
      maxSize = n
    }

-- | Make a new queue from a maximum size and a list. If the list is larger than
-- the maximum size, items will be dropped from the back of the input list.
fromList :: Int -> [a] -> Queue a
fromList n xs =
  Queue
    { front = reverse xs',
      back = [],
      size = l',
      maxSize = n
    }
  where
    l = length xs
    (xs', l') = if l > n then (take n xs, n) else (xs, l)

-- | Reverse the \'back\' list and assign it to \'front\'. O(n)
--
-- NOTE: This will error if \'front\' is not empty.
reverseBack :: Queue a -> Queue a
reverseBack q =
  if not (isFrontEmpty q)
    then error "reverseBack: front was not empty"
    else
      Queue
        { front = reverse (back q),
          back = [],
          size = size q,
          maxSize = maxSize q
        }

-- | Check if front is empty
isFrontEmpty :: Queue a -> Bool
isFrontEmpty = null . front

-- | Remove one item from the front of the queue.
dequeue :: Queue a -> (Maybe a, Queue a)
dequeue q =
  case front q' of
    [] -> (Nothing, q')
    (x : xs) ->
      ( Just x,
        Queue
          { front = xs,
            back = back q',
            size = size q' - 1,
            maxSize = maxSize q'
          }
      )
  where
    q' = if isFrontEmpty q then reverseBack q else q

-- | Add one item to the back of the queue.
enqueue :: a -> Queue a -> Queue a
enqueue a q =
  Queue
    { front = front q',
      back = a : back q',
      size = size q' + 1,
      maxSize = maxSize q'
    }
  where
    q' = if size q == maxSize q then snd (dequeue q) else q

-- | From an input list, determine which inputs have been seen before, and
-- discard them. Enqueue the new items. Return the resulting queue as well as
-- a reversed list of the unique items.
merge ::
  (Eq a) =>
  -- | The items to potentially be added.
  [a] ->
  -- | The existing queue, containing \'seen\' items.
  Queue a ->
  -- | The resulting queue after unique items are added, and the unique items themselves.
  -- Note that the unique items are reversed.
  (Queue a, [a])
merge inputs q =
  let acc (q', unique) item =
        if item `elem` q'
          then (q', unique)
          else (enqueue item q', item : unique)
   in foldl' acc (q, []) inputs

-- | Map over all elements.
instance Functor Queue where
  fmap f q =
    Queue
      { front = fmap f (front q),
        back = fmap f (back q),
        size = size q,
        maxSize = maxSize q
      }

-- | Folds happen from front to back.
instance Foldable Queue where
  foldMap f q = foldMap f (front q) <> foldMap f (reverse $ back q)
