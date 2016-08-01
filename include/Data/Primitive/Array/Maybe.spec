module spec Data.Primitive.Array.Maybe where

import GHC.Base

data variance Data.Primitive.Array.Maybe.MutableMaybeArray invariant covariant

measure mmalen :: Data.Primitive.Array.Maybe.MutableMaybeArray s a -> Int

invariant {v : MutablePrimArray s a | 0 <= mmalen v }

assume newMaybeArray :: PrimMonad m
  => n:Nat 
  -> a
  -> m {v:(Data.Primitive.Array.Maybe.MutableMaybeArray (PrimState m) a) | n = mmalen v}

assume readMaybeArray :: PrimMonad m
  => v:(Data.Primitive.Array.Maybe.MutableMaybeArray (PrimState m) a) 
  -> {n:Nat | n < mmalen v} 
  -> m (Maybe a)

assume writePrimArray :: (Prim a, PrimMonad m) 
  => v:(Data.Primitive.Array.Maybe.MutableMaybeArray (PrimState m) a) 
  -> {n:Nat | n < mmalen v} -> a -> m () 


