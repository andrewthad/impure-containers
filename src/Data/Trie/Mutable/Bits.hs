{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Trie.Mutable.Bits where

import Control.Monad.Primitive
import Data.Primitive.ByteArray
import Data.Primitive.Array
import Data.Word
import GHC.TypeLits
import Data.Primitive.PrimArray
import Data.Primitive.Bool (BoolByte(..))

-- newtype BoolPositiveArray s = BoolBinTree (MutableByteArray s)
-- newtype ValuePositiveArray s a = ValuePositiveArray (MutableArray s a)
-- 
-- newtype BoolArray s = BoolArray (MutablePrimArray s Word8)
-- newtype TrieArray s k v = TrieArray 
--   (MutableArray s (Trie s k v))

{-@

data Trie s k v = Trie
  { trieHasValue :: { x:MutablePrimArray s BoolByte | mpalen x = 15 }
  , trieValue    :: { x:MutableArray s v            | mpalen x = 15 }
  , trieHasChild :: { x:MutablePrimArray s BoolByte | mpalen x = 16 }
  , trieChildren :: { x:MutableArray s (Trie s k v) | mpalen x = 16 }
  }

@-}

data Trie s k v = Trie
  { trieHasValue :: !(MutablePrimArray s BoolByte)
  , trieValue    :: !(MutableArray s v)
  , trieHasChild :: !(MutablePrimArray s BoolByte)
  , trieChildren :: !(MutableArray s (Trie s k v))
  }

-- | Must be a multiple of two.
{-@ measure theSize :: {i:Int|i = 16} @-}
theSize :: Int
theSize = 16

{-@ measure theSmallSize :: {i:Int|i = theSize - 1} @-}
theSmallSize :: Int
theSmallSize = 15

new :: PrimMonad m => m (Trie (PrimState m) k v)
new = Trie
  <$> ( do x <- newPrimArray theSmallSize
           setPrimArray x 0 theSmallSize (BoolByte False)
           return x
      )
  <*> newArray 15 valError
  <*> ( do x <- newPrimArray theSize
           setPrimArray x 0 theSize (BoolByte False)
           return x
      )
  <*> newArray 16 trieError
  where (valError,trieError) = theErrors

theErrors :: (v, Trie s k v)
theErrors = ( error "mutable trie: valError forced"
            , error "mutable trie: trieError forced"
            )

{-@ type Positive   = {n:Int      | n > 0            } @-}
{-@ type SmallIndex = {n:Positive | n < theSmallSize } @-}
{-@ type Index      = {n:Positive | n < theSize      } @-}

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


