{-# LANGUAGE MagicHash #-}

module Data.Primitive.MutVar.Maybe
  ( MutMaybeVar
  , newMutMaybeVar
  , readMutMaybeVar
  , writeMutMaybeVar
  ) where

import           Control.Monad.Primitive
import           Data.Primitive.MutVar

import           GHC.Prim
import           GHC.Types
import           Unsafe.Coerce

import           Data.Maybe

newtype MutMaybeVar s a = MutMaybeVar (MutVar s Any)

-- | nothingSurrogate stands in for the value Nothing; we distinguish it by pointer
nothingSurrogate :: Any
nothingSurrogate = error "Data.Primitive.MutVar.Maybe.nothingSurrogate evaluated"
{-# NOINLINE nothingSurrogate #-}

newMutMaybeVar :: PrimMonad m => Maybe a -> m (MutMaybeVar (PrimState m) a)
newMutMaybeVar ma = case ma of
  Just a -> do
    x <- newMutVar (unsafeCoerce a)
    return (MutMaybeVar x)
  Nothing -> do
    x <- newMutVar nothingSurrogate
    return (MutMaybeVar x)
{-# INLINE newMutMaybeVar #-}

readMutMaybeVar :: PrimMonad m => MutMaybeVar (PrimState m) a -> m (Maybe a)
readMutMaybeVar (MutMaybeVar r) = do
  x <- readMutVar r
  return $ toMaybe x
{-# INLINE readMutMaybeVar #-}

toMaybe :: Any -> Maybe a
toMaybe x = case reallyUnsafePtrEquality# x nothingSurrogate of
  0# -> Just $ unsafeCoerce x
  _ -> Nothing
{-# INLINE toMaybe #-}

writeMutMaybeVar :: PrimMonad m => MutMaybeVar (PrimState m) a -> Maybe a -> m ()
writeMutMaybeVar (MutMaybeVar r) ma = case ma of
  Just a -> writeMutVar r $ unsafeCoerce a
  Nothing -> writeMutVar r nothingSurrogate
{-# INLINE writeMutMaybeVar #-}

