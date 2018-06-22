module spec Data.Primitive.PrimArray where

import GHC.Base

data variance Data.Primitive.PrimArray.MutablePrimArray invariant covariant

measure mpalen :: MutablePrimArray s a -> Int

invariant {v : MutablePrimArray s a | 0 <= mpalen v }

assume newPrimArray :: (PrimMonad m, Prim a)
  => n:Nat
  -> m {v:(MutablePrimArray (PrimState m) a) | n = mpalen v}

assume readPrimArray :: (Prim a, PrimMonad m)
  => v:(MutablePrimArray (PrimState m) a)
  -> {n:Nat | n < mpalen v}
  -> m a

assume writePrimArray :: (Prim a, PrimMonad m)
  => v:(MutablePrimArray (PrimState m) a)
  -> {n:Nat | n < mpalen v} -> a -> m ()

assume sizeofMutablePrimArray :: Prim a
  => v:(MutablePrimArray s a)
  -> {n : Nat | n = mpalen v}

assume setPrimArray :: (Prim a, PrimMonad m)
  => v:(MutablePrimArray (PrimState m) a)
  -> start:Nat
  -> {count:Nat | start + count < mpalen v }
  -> a
  -> m ()
