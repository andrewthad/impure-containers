{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Containers.Impure.Internal where

import Control.Monad.Primitive
import Data.Primitive.ByteArray
import Data.Primitive.Types
import GHC.Prim (newByteArray#,quotInt#,sizeofMutableByteArray#,(*#))
import GHC.Types (Int(..))

newtype MutablePrimArray s a = MutablePrimArray (MutableByteArray s)

-- A hack to avoid adding forall statements to
-- other type signatures.
toUndefined1 :: m (MutablePrimArray (PrimState m) a) -> a
toUndefined1 _ = undefined

toUndefined2 :: MutablePrimArray s a -> a
toUndefined2 _ = undefined

newPrimArray :: (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
newPrimArray (I# n#) = result
  where
  result = primitive (\s# -> case newByteArray# (n# *# (sizeOf# (toUndefined1 result))) s# of
      (# s'#, arr# #) -> (# s'#, MutablePrimArray (MutableByteArray arr#) #)
    )

-- | Size of the mutable prim array.
sizeofMutablePrimArray :: Prim a => MutablePrimArray s a -> Int
{-# INLINE sizeofMutablePrimArray #-}
sizeofMutablePrimArray p@(MutablePrimArray (MutableByteArray arr#)) =
  I# (quotInt# (sizeofMutableByteArray# arr#) (sizeOf# (toUndefined2 p)))

