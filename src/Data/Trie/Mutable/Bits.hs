{-# LANGUAGE BangPatterns #-}

module Data.Trie.Mutable.Bits
  ( MTrie(..)
  , new
  , lookup
  , insert
  , insertPrefix
  ) where

import           Control.Monad.Primitive
import           Data.Bits
import           Data.Primitive.ByteArray
import           Data.Primitive.MutVar.Maybe
import           Prelude                     hiding (lookup)

data MTrie s k v = MTrie
  { mtrieValue :: !(MutMaybeVar s v)
  , mtrieLeft  :: !(MutMaybeVar s (MTrie s k v))
  , mtrieRight :: !(MutMaybeVar s (MTrie s k v))
  }

new :: PrimMonad m => m (MTrie (PrimState m) k v)
new = MTrie
  <$> newMutMaybeVar Nothing
  <*> newMutMaybeVar Nothing
  <*> newMutMaybeVar Nothing

-- | This gives the best match, that is, the
--   value stored at the longest prefix that
--   matched this key.
lookup :: (FiniteBits k, PrimMonad m)
  => MTrie (PrimState m) k v
  -> k
  -> m (Maybe v)
lookup theTrie theKey = go Nothing theTrie theKey where
  totalBits :: Int
  totalBits = finiteBitSize theKey
  -- mask :: k
  mask = bit (totalBits - 1)
  -- zero :: k
  zero = zeroBits
  go !mres (MTrie valRef leftRef rightRef) key = do
    mval <- readMutMaybeVar valRef
    let mresNext = case mval of
          Nothing -> mres
          Just res -> Just res
        chosenRef = if (mask .&. key) == zero
          then leftRef
          else rightRef
    chosen <- readMutMaybeVar chosenRef
    case chosen of
      Nothing -> return mresNext
      Just nextTrie -> go mresNext nextTrie (unsafeShiftL key 1)

insert :: (FiniteBits k, PrimMonad m)
  => MTrie (PrimState m) k v
  -> k -- ^ prefix key
  -> v -- ^ value
  -> m ()
insert trie key = insertPrefix trie (finiteBitSize key) key

insertPrefix :: (FiniteBits k, PrimMonad m)
  => MTrie (PrimState m) k v
  -> Int -- ^ significant bits from key
  -> k -- ^ prefix key
  -> v -- ^ value
  -> m ()
insertPrefix theTrie theSig theKey value =
  if theSig > totalBits
    then return ()
    else go theSig theKey theTrie
  where
  totalBits :: Int
  totalBits = finiteBitSize theKey
  -- mask :: k
  mask = bit (totalBits - 1)
  -- zero :: k
  zero = zeroBits
  go !significant !key (MTrie valRef leftRef rightRef) = if significant > 0
    then do
      let chosenRef = if (mask .&. key) == zero
            then leftRef
            else rightRef
      chosen <- readMutMaybeVar chosenRef
      nextTrie <- case chosen of
        Nothing -> do
          nextTrie <- new
          writeMutMaybeVar chosenRef (Just nextTrie)
          return nextTrie
        Just nextTrie -> return nextTrie
      go (significant - 1) (unsafeShiftL key 1) nextTrie
    else writeMutMaybeVar valRef (Just value)



