{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Primitive.PrimArray where

import Control.Monad.Primitive
import Data.Primitive.ByteArray
import Data.Primitive.Types
import GHC.Prim (newByteArray#,quotInt#,sizeofMutableByteArray#,(*#))
import GHC.Types (Int(..))

newtype MutablePrimArray s a = MutablePrimArray (MutableByteArray s)

newPrimArray :: (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
newPrimArray (I# n#) = result
  where
  result = primitive (\s# -> case newByteArray# (n# *# (sizeOf# (toUndefined1 result))) s# of
      (# s'#, arr# #) -> (# s'#, MutablePrimArray (MutableByteArray arr#) #)
    )

readPrimArray :: (Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> Int -> m a
readPrimArray (MutablePrimArray m) = readByteArray m
{-# INLINE readPrimArray #-}

writePrimArray :: (Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> Int -> a -> m ()
writePrimArray (MutablePrimArray m) = writeByteArray m
{-# INLINE writePrimArray #-}

sizeofMutablePrimArray :: Prim a => MutablePrimArray s a -> Int
sizeofMutablePrimArray p@(MutablePrimArray (MutableByteArray arr#)) =
  I# (quotInt# (sizeofMutableByteArray# arr#) (sizeOf# (toUndefined2 p)))
{-# INLINE sizeofMutablePrimArray #-}

setPrimArray
  :: (Prim a, PrimMonad m) 
  => MutablePrimArray (PrimState m) a -- ^ array to fill
  -> Int -- ^ offset into array
  -> Int -- ^ number of values to fill
  -> a   -- ^ value to fill with
  -> m ()
setPrimArray (MutablePrimArray m) = setByteArray m

-- A hack to avoid adding forall statements to
-- other type signatures.
toUndefined1 :: m (MutablePrimArray (PrimState m) a) -> a
toUndefined1 _ = undefined

toUndefined2 :: MutablePrimArray s a -> a
toUndefined2 _ = undefined


