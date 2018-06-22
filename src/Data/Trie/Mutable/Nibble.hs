{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Trie.Mutable.Nibble where

import           Control.Monad.Primitive
import           Data.Primitive.Array
import           Data.Primitive.Array.Maybe
import           Data.Primitive.Bool        (BoolByte (..))
import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import           Data.Word
import           GHC.TypeLits

-- newtype BoolPositiveArray s = BoolBinTree (MutableByteArray s)
-- newtype ValuePositiveArray s a = ValuePositiveArray (MutableArray s a)
--
-- newtype BoolArray s = BoolArray (MutablePrimArray s Word8)
-- newtype TrieArray s k v = TrieArray
--   (MutableArray s (Trie s k v))

-- | Must be a multiple of two.
{-@ measure theSize :: {i:Int|i = 16} @-}
theSize :: Int
theSize = 16

{-@ measure theSmallSize :: {i:Int|i = theSize - 1} @-}
theSmallSize :: Int
theSmallSize = 15

{-@ type Positive   = {n:Int      | n > 0            } @-}
{-@ type SmallIndex = {n:Positive | n < theSmallSize } @-}
{-@ type Index      = {n:Positive | n < theSize      } @-}

{-@

data Trie s k v = Trie
  { trieValue    :: { x:MutableMaybeArray s v            | mmalen x = theSmallSize }
  , trieChildren :: { x:MutableMaybeArray s (Trie s k v) | mmalen x = theSize }
  }

@-}

data Trie s k v = Trie
  { trieValue    :: !(MutableMaybeArray s v)
  , trieChildren :: !(MutableMaybeArray s (Trie s k v))
  }

new :: PrimMonad m => m (Trie (PrimState m) k v)
new = Trie
  <$> newMaybeArray 15 Nothing
  <*> newMaybeArray 16 Nothing

insertPrefix :: FiniteBits k
  => Trie (PrimState m) k v
  -> k -- ^ prefix key
  -> Int -- ^ significant bits from key
  -> v -- ^ value
  -> m ()
insertPrefix (Trie vals children)

  -- where (valError,trieError) = theErrors

-- theErrors :: (v, Trie s k v)
-- theErrors = ( error "mutable trie: valError forced"
--             , error "mutable trie: trieError forced"
--             )

-- -- | All initialized to false
-- newBoolArray :: PrimMonad m => m (BoolArray (PrimState m))
-- newBoolArray = do
--   bs <- newByteArray theSize
--   setByteArray bs 0 theSize (0 :: Word8)
--   return (BoolArray bs)
--
-- boolToWord8 :: Bool -> Word8
-- boolToWord8 x = case x of
--   True -> 1
--   False -> 0
--
-- word8Zero :: Word8 -> Bool
-- word8Zero w = w == 0
-- {-# INLINE word8Zero #-}
--
-- {-@ writeBoolArray :: BoolArray (PrimState m) -> {n:Positive |n < theSize}  -> Bool -> m () @-}
-- writeBoolArray :: PrimMonad m => BoolArray (PrimState m) -> Int -> Bool -> m ()
-- writeBoolArray (BoolArray m) i b = writePrimArray m i (boolToWord8 b)
-- {-# INLINE writeBoolArray #-}
--
-- readBoolArray :: PrimMonad m => BoolArray (PrimState m) -> Int -> m Bool
-- readBoolArray (BoolArray m) i = do
--   v <- readPrimArray m i
--   return $ if word8Zero v
--     then False
--     else True
-- {-# INLINE readBoolArray #-}
