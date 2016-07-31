module spec Data.Primitive.Array.Maybe where

import GHC.Base

data variance Data.Primitive.Array.Mutable invariant covariant

measure malen :: MutablePrimArray s a -> Int

invariant {v : MutablePrimArray s a | 0 <= malen v }

assume newMaybeArray :: PrimMonad m
  => n:Nat 
  -> a
  -> m {v:(MutableMaybeArray (PrimState m) a) | n = malen v}

assume readMaybeArray :: PrimMonad m
  => v:(MutableMaybeArray (PrimState m) a) 
  -> {n:Nat | n < malen v} 
  -> m (Maybe a)

assume writePrimArray :: (Prim a, PrimMonad m) 
  => v:(MutableMaybeArray (PrimState m) a) 
  -> {n:Nat | n < malen v} -> a -> m () 


