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

newtype MutableMaybeArray s a = MutableMaybeArray (MutableArray s a)

unsafeFromMaybe :: Maybe a -> a
unsafeFromMaybe x = case x of
  Nothing -> elementNothing
  Just a -> a
{-# INLINE unsafeFromMaybe #-}

unsafeToMaybe :: a -> Maybe a
unsafeToMaybe a = 
  case reallyUnsafePtrEquality# a elementNothing of
    0# -> Just a
    _  -> Nothing
{-# INLINE unsafeToMaybe #-}

elementNothing :: a
elementNothing = error "elementNothing: This value should not be forced!"
{-# NOINLINE elementNothing #-}

newMaybeArray :: PrimMonad m => Int -> Maybe a -> m (MutableMaybeArray (PrimState m) a)
newMaybeArray i ma = do
  x <- newArray i (unsafeFromMaybe ma)
  return (MutableMaybeArray x)

readMaybeArray :: PrimMonad m => MutableMaybeArray (PrimState m) a -> Int -> m (Maybe a)
readMaybeArray (MutableMaybeArray m) ix = do
  a <- readArray m ix
  return (unsafeToMaybe a)

writeMaybeArray :: PrimMonad m => MutableMaybeArray (PrimState m) a -> Int -> Maybe a -> m ()
writeMaybeArray (MutableMaybeArray marr) ix ma = 
  writeArray marr ix (unsafeFromMaybe ma)


