{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Containers.Impure.Internal where

import           Control.Monad.Primitive
import           Data.Primitive.ByteArray
import           Data.Primitive.Types
import           GHC.Prim
                 (newByteArray#, quotInt#, sizeofMutableByteArray#, (*#))
import           GHC.Types                (Int (..))

{-@ measure mpalen :: MutablePrimArray s a -> Int @-}
{-@ invariant {v : MutablePrimArray s a | 0 <= mpalen v } @-}

newtype MutablePrimArray s a = MutablePrimArray (MutableByteArray s)

{-@ assume newPrimArray :: (PrimMonad m, Prim a) => n:Nat
                        -> m {v:(MutablePrimArray (PrimState m) a) | n = mpalen v}
@-}
newPrimArray :: (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
newPrimArray (I# n#) = result
  where
  result = primitive (\s# -> case newByteArray# (n# *# (sizeOf# (toUndefined1 result))) s# of
      (# s'#, arr# #) -> (# s'#, MutablePrimArray (MutableByteArray arr#) #)
    )

{-@ assume readPrimArray :: (Prim a, PrimMonad m) => v:(MutablePrimArray (PrimState m) a) -> {n:Nat | n < mpalen v} -> m a @-}
readPrimArray :: (Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> Int -> m a
readPrimArray (MutablePrimArray m) = readByteArray m
{-# INLINE readPrimArray #-}

{-@ assume writePrimArray :: (Prim a, PrimMonad m)
      => v:(MutablePrimArray (PrimState m) a)
      -> {n:Nat | n < mpalen v} -> a -> m ()
@-}
writePrimArray :: (Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> Int -> a -> m ()
writePrimArray (MutablePrimArray m) = writeByteArray m
{-# INLINE writePrimArray #-}

{-@ assume sizeofMutablePrimArray :: Prim a => v:(MutablePrimArray s a) -> {n : Nat | n = mpalen v} @-}
-- | Size of the mutable prim array.
sizeofMutablePrimArray :: Prim a => MutablePrimArray s a -> Int
sizeofMutablePrimArray p@(MutablePrimArray (MutableByteArray arr#)) =
  I# (quotInt# (sizeofMutableByteArray# arr#) (sizeOf# (toUndefined2 p)))
{-# INLINE sizeofMutablePrimArray #-}


-- A hack to avoid adding forall statements to
-- other type signatures.
toUndefined1 :: m (MutablePrimArray (PrimState m) a) -> a
toUndefined1 _ = undefined

toUndefined2 :: MutablePrimArray s a -> a
toUndefined2 _ = undefined

