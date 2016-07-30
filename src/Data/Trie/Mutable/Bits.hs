{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Trie.Mutable.Bits where

import Control.Monad.Primitive
import Data.Primitive.ByteArray
import Data.Primitive.Array
import Data.Word
import GHC.TypeLits
import Data.Primitive.PrimArray

newtype BoolPositiveArray s = BoolBinTree (MutableByteArray s)
newtype ValuePositiveArray s a = ValuePositiveArray (MutableArray s a)

newtype BoolArray s = BoolArray (MutablePrimArray s Word8)
newtype TrieArray s k v = TrieArray (MutableArray s (Trie s k v))

{-@

data Trie s k v = Trie
  { trieHasValue :: !(BoolPositiveArray s)
  , trieValue    :: !(MutableArray s v)
  , trieHasChild :: !(BoolArray s)
  , trieChildren :: !(TrieArray s k v)
  }

@-}

data Trie s k v = Trie
  { trieHasValue :: !(BoolArray s)
  , trieValue    :: !(MutableArray s v)
  , trieHasChild :: !(BoolArray s)
  , trieChildren :: !(TrieArray s k v)
  }

-- | Must be a multiple of two.
{-@ measure theSize :: {i:Int|i = 16} @-}
theSize :: Int
theSize = 16

{-@ type Positive = {n:Int|n > 0} @-}
{-@ type Index = {n:Int|n > 0} @-}

-- | All initialized to false
newBoolArray :: PrimMonad m => m (BoolArray (PrimState m))
newBoolArray = do
  bs <- newByteArray theSize
  setByteArray bs 0 theSize (0 :: Word8)
  return (BoolArray bs)

boolToWord8 :: Bool -> Word8
boolToWord8 x = case x of
  True -> 1
  False -> 0

word8Zero :: Word8 -> Bool
word8Zero w = w == 0
{-# INLINE word8Zero #-}

{-@ writeBoolArray :: BoolArray (PrimState m) -> {n:Positive |n < theSize}  -> Bool -> m () @-}
writeBoolArray :: PrimMonad m => BoolArray (PrimState m) -> Int -> Bool -> m ()
writeBoolArray (BoolArray m) i b = writePrimArray m i (boolToWord8 b)
{-# INLINE writeBoolArray #-}

readBoolArray :: PrimMonad m => BoolArray (PrimState m) -> Int -> m Bool
readBoolArray (BoolArray m) i = do
  v <- readPrimArray m i
  return $ if word8Zero v
    then False
    else True
{-# INLINE readBoolArray #-}


