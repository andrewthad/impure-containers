{-# LANGUAGE MagicHash #-}

-- | This uses some unsafe hackery.
module Data.Primitive.Array.Maybe 
  ( MutableMaybeArray
  , newMaybeArray
  , readMaybeArray
  , writeMaybeArray
  ) where

import Control.Monad.Primitive
import Data.Primitive.Array
import GHC.Prim (reallyUnsafePtrEquality#)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

newtype MutableMaybeArray s a = MutableMaybeArray (MutableArray s Any)

unsafeToMaybe :: Any -> Maybe a
unsafeToMaybe a = 
  case reallyUnsafePtrEquality# a nothingSurrogate of
    0# -> Just (unsafeCoerce a)
    _  -> Nothing
{-# INLINE unsafeToMaybe #-}

nothingSurrogate :: Any
nothingSurrogate = error "nothingSurrogate: This value should not be forced!"
{-# NOINLINE nothingSurrogate #-}

newMaybeArray :: PrimMonad m => Int -> Maybe a -> m (MutableMaybeArray (PrimState m) a)
newMaybeArray i ma = case ma of
  Just a -> do
    x <- newArray i (unsafeCoerce a)
    return (MutableMaybeArray x)
  Nothing -> do 
    x <- newArray i nothingSurrogate
    return (MutableMaybeArray x)

readMaybeArray :: PrimMonad m => MutableMaybeArray (PrimState m) a -> Int -> m (Maybe a)
readMaybeArray (MutableMaybeArray m) ix = do
  a <- readArray m ix
  return (unsafeToMaybe a)

writeMaybeArray :: PrimMonad m => MutableMaybeArray (PrimState m) a -> Int -> Maybe a -> m ()
writeMaybeArray (MutableMaybeArray marr) ix ma = case ma of
  Just a -> writeArray marr ix (unsafeCoerce a)
  Nothing -> writeArray marr ix nothingSurrogate


