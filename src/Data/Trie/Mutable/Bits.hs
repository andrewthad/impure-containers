{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Trie.Mutable.Bits where

import Control.Monad.Primitive
import Data.Primitive.ByteArray
import Data.Primitive.Array
import Data.Word
import GHC.TypeLits

data Thing (a :: Nat) = Thing

newtype BoolPositiveArray s = BoolBinTree (MutableByteArray s)
newtype ValuePositiveArray s a = ValuePositiveArray (MutableArray s a)
newtype BoolArray s = BoolArray (MutableByteArray s)
newtype TrieArray s k v = TrieArray (MutableArray s (Trie s k v))

data Trie s k v = Trie
  { trieHasValue :: !(BoolPositiveArray s)
  , trieValue    :: !(ValuePositiveArray s v)
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
writeBoolPositiveArray :: PrimMonad m => BoolArray (PrimState m) -> Int -> Bool -> m ()
writeBoolPositiveArray (BoolArray m) i b = writeByteArray m i (boolToWord8 b)
{-# INLINE writeBoolArray #-}

readBoolPositiveArray :: PrimMonad m => BoolArray (PrimState m) -> Int -> m Bool
readBoolPositiveArray (BoolArray m) i = do
  v <- readByteArray m i
  return $ if word8Zero v
    then False
    else True
{-# INLINE readBoolArray #-}


