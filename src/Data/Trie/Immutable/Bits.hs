{-# LANGUAGE BangPatterns #-}

module Data.Trie.Immutable.Bits
  ( Trie(..)
  , empty
  , lookup
  , freeze
  ) where

import Control.Monad.Primitive
import Data.Bits
import Data.Primitive.ByteArray
import Data.Primitive.MutVar.Maybe
import Data.Trie.Mutable.Bits (MTrie(..))

data Trie k v = Trie
  { trieValue :: !(Maybe v)
  , trieLeft  :: !(Maybe (Trie k v))
  , trieRight :: !(Maybe (Trie k v))
  }

empty :: Trie k v
empty = Trie Nothing Nothing Nothing

-- | This gives the best match, that is, the
--   value stored at the longest prefix that
--   matched this key.
lookup :: FiniteBits k
  => Trie k v
  -> k
  -> Maybe v
lookup theTrie theKey = go Nothing theTrie theKey where
  totalBits :: Int
  totalBits = finiteBitSize theKey
  -- mask :: k
  mask = bit (totalBits - 1)
  -- zero :: k
  zero = zeroBits
  go !mres (Trie mval mleft mright) key =
    let chosen = if (mask .&. key) == zero then mleft else mright
     in case chosen of
          Nothing -> mval
          Just nextTrie -> go mval nextTrie (unsafeShiftL key 1)

freeze :: PrimMonad m => MTrie (PrimState m) k v -> m (Trie k v)
freeze = go where
  go (MTrie valVar leftVar rightVar) = do
    mleft  <- readMutMaybeVar leftVar
    mright <- readMutMaybeVar rightVar
    mval   <- readMutMaybeVar valVar
    immutableLeft <- case mleft of
      Just left -> fmap Just $ go left
      Nothing -> return Nothing
    immutableRight <- case mright of
      Just right -> fmap Just $ go right
      Nothing -> return Nothing
    return (Trie mval immutableLeft immutableRight)

