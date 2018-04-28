--------------------------------------------------------------------------------

-- License:
--     Copyright 2015 Edward Kmett
--     Copyright 2018 Remy Goldschmidt
--
--     All rights reserved.
--
--     Redistribution and use in source and binary forms, with or without
--     modification, are permitted provided that the following conditions
--     are met:
--
--     1. Redistributions of source code must retain the above copyright
--        notice, this list of conditions and the following disclaimer.
--
--     2. Redistributions in binary form must reproduce the above copyright
--        notice, this list of conditions and the following disclaimer in the
--        documentation and/or other materials provided with the distribution.
--
--     THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
--     IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
--     WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--     DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
--     ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
--     DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
--     OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
--     HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
--     STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
--     ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--     POSSIBILITY OF SUCH DAMAGE.

--------------------------------------------------------------------------------

{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples   #-}
{-# LANGUAGE Unsafe          #-}

--------------------------------------------------------------------------------

-- | Unboxed primitive references.
--
--   Note: Edward Kmett wrote everything in this module. It was sitting
--   unpackaged on GitHub, so I took it and published it as a part of
--   this package.
module ImpureContainers.PrimRef
  ( -- * 'PrimRef'
    PrimRef

    -- ** Creation
  , new
  , newPinned
  , newAlignedPinned

    -- ** Simple functions
  , read
  , write
  , contents

    -- ** Atomic mutators
  , atomicReadInt
  , atomicWriteInt
  , casInt
  , fetchAddInt
  , fetchSubInt
  , fetchAndInt
  , fetchNandInt
  , fetchOrInt
  , fetchXorInt

    -- ** Unsafe functions
  , unsafeToMByteArray
  , unsafeFromMByteArray
  ) where

--------------------------------------------------------------------------------

import           Prelude                     ()

import           Control.Applicative         (pure)
import           Data.Eq                     (Eq ((==)))
import           Data.Function               (($))

import           Control.Monad.Primitive
                 (PrimMonad, PrimState, primitive, primitive_)

import           Data.Primitive              (Addr, Prim, alignment, sizeOf)

import qualified GHC.Prim                    as GHC.Prim
import           GHC.Types                   (Int (I#))

import           ImpureContainers.MByteArray (MByteArray)
import qualified ImpureContainers.MByteArray as MByteArray

--------------------------------------------------------------------------------

-- FIXME: when GHC supports creating newtypes in kind #, we should create a
--        type called PrimRef# to reduce indirections even further.
--        see https://ghc.haskell.org/trac/ghc/ticket/1311 for progress.

-- | FIXME: doc
newtype PrimRef s a
  = MkPrimRef (MByteArray s)

type role PrimRef nominal nominal

--------------------------------------------------------------------------------

-- | Create a primitive reference.
new
  :: (PrimMonad m, Prim a)
  => a
  -- ^ FIXME: doc
  -> m (PrimRef (PrimState m) a)
  -- ^ FIXME: doc
new a = do
  m <- MByteArray.new (sizeOf a)
  MByteArray.write m 0 a
  pure (MkPrimRef m)
{-# INLINE new #-}

-- | Create a pinned primitive reference.
newPinned
  :: (PrimMonad m, Prim a)
  => a
  -- ^ FIXME: doc
  -> m (PrimRef (PrimState m) a)
  -- ^ FIXME: doc
newPinned a = do
  m <- MByteArray.newPinned (sizeOf a)
  MByteArray.write m 0 a
  pure (MkPrimRef m)
{-# INLINE newPinned #-}

-- | Create a pinned primitive reference with the appropriate alignment for
--   its contents.
newAlignedPinned
  :: (PrimMonad m, Prim a)
  => a
  -- ^ FIXME: doc
  -> m (PrimRef (PrimState m) a)
  -- ^ FIXME: doc
newAlignedPinned a = do
  m <- MByteArray.newAlignedPinned (sizeOf a) (alignment a)
  MByteArray.write m 0 a
  pure (MkPrimRef m)
{-# INLINE newAlignedPinned #-}

-- | Read a primitive value from the reference
read
  :: (PrimMonad m, Prim a)
  => PrimRef (PrimState m) a
  -- ^ FIXME: doc
  -> m a
  -- ^ FIXME: doc
read (MkPrimRef m) = MByteArray.read m 0
{-# INLINE read #-}

-- | Write a primitive value to the reference
write
  :: (PrimMonad m, Prim a)
  => PrimRef (PrimState m) a
  -- ^ FIXME: doc
  -> a
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
write (MkPrimRef m) a = MByteArray.write m 0 a
{-# INLINE write #-}

-- | FIXME: doc
instance Eq (PrimRef s a) where
  (MkPrimRef m) == (MkPrimRef n) = MByteArray.same m n
  {-# INLINE (==) #-}

-- | Yield a pointer to the data of a 'PrimRef'.
--
--   This operation is only safe on pinned byte arrays allocated by
--   'newPinned' or 'newAlignedPinned'.
contents
  :: PrimRef s a
  -- ^ FIXME: doc
  -> Addr
  -- ^ FIXME: doc
contents (MkPrimRef m) = MByteArray.contents m
{-# INLINE contents #-}

--------------------------------------------------------------------------------

-- | Given a reference, read an element.
--
--   Implies a full memory barrier.
atomicReadInt
  :: (PrimMonad m)
  => PrimRef (PrimState m) Int
  -- ^ A primitive reference.
  -> m Int
  -- ^ A 'PrimMonad' action that reads the primitive reference and returns
  --   its current value as an 'Int'.
atomicReadInt (MkPrimRef mba) = primitive $ \s -> do
  let m = MByteArray.underlyingMutableByteArray# mba
  case GHC.Prim.atomicReadIntArray# m 0# s of
    (# s', result #) -> (# s', I# result #)

-- | Given a reference, write an element.
--
--   Implies a full memory barrier.
atomicWriteInt
  :: (PrimMonad m)
  => PrimRef (PrimState m) Int
  -- ^ A primitive reference.
  -> Int
  -- ^ The new value the primitive reference should take on.
  -> m ()
  -- ^ A 'PrimMonad' action that writes the given 'Int' to the given
  --   primitive reference.
atomicWriteInt (MkPrimRef mba) (I# x) = primitive_ $ \s -> do
  let m = MByteArray.underlyingMutableByteArray# mba
  GHC.Prim.atomicWriteIntArray# m 0# x s

-- | Given a primitive reference, the expected old value, and the new value,
--   perform an atomic compare-and-swap, i.e.: write the new value if the
--   current value matches the provided old value.
--
--   Returns the value of the element before the operation.
--
--   Implies a full memory barrier.
casInt
  :: (PrimMonad m)
  => PrimRef (PrimState m) Int
  -- ^ A primitive reference.
  -> Int
  -- ^ The expected old value.
  -> Int
  -- ^ The new value.
  -> m Int
  -- ^ A 'PrimMonad' action that atomically writes the new value to the
  --   primitive reference if the current value matches the expected old value,
  --   and then returns the old value of the primitive reference.
casInt (MkPrimRef mba) (I# oldValue) (I# newValue) = primitive $ \s -> do
  let m = MByteArray.underlyingMutableByteArray# mba
  case GHC.Prim.casIntArray# m 0# oldValue newValue s of
    (# s', result #) -> (# s', I# result #)

-- | Given a reference, and a value to add, atomically add the value to the
--   element.
--
--   Returns the value of the element before the operation.
--
--   Implies a full memory barrier.
fetchAddInt
  :: (PrimMonad m)
  => PrimRef (PrimState m) Int
  -- ^ A primitive reference.
  -> Int
  -- ^ An 'Int' to add to the current value of the primitive reference.
  -> m Int
  -- ^ A 'PrimMonad' action that atomically sets the value of the given
  --   primitive reference to the result of adding the given 'Int' to its
  --   current value, and then returns the value the primitive reference
  --   had before this modification.
fetchAddInt (MkPrimRef mba) (I# x) = primitive $ \s -> do
  let m = MByteArray.underlyingMutableByteArray# mba
  case GHC.Prim.fetchAddIntArray# m 0# x s of
    (# s', result #) -> (# s', I# result #)

-- | Given a reference and a value to subtract, atomically subtract the value
--   from the element.
--
--   Returns the value of the element before the operation.
--
--   Implies a full memory barrier.
fetchSubInt
  :: (PrimMonad m)
  => PrimRef (PrimState m) Int
  -- ^ A primitive reference.
  -> Int
  -- ^ An 'Int' to subtract from the current value of the primitive reference.
  -> m Int
  -- ^ A 'PrimMonad' action that atomically sets the value of the given
  --   primitive reference to the result of subtracting the given 'Int' from
  --   its current value, and then returns the value the primitive reference
  --   had before this modification.
fetchSubInt (MkPrimRef mba) (I# x) = primitive $ \s -> do
  let m = MByteArray.underlyingMutableByteArray# mba
  case GHC.Prim.fetchSubIntArray# m 0# x s of
    (# s', result #) -> (# s', I# result #)

-- | Given a reference and a value with which to bitwise AND, atomically AND
--   the value with the element.
--
--   Returns the value of the element before the operation.
--
--   Implies a full memory barrier.
fetchAndInt
  :: (PrimMonad m)
  => PrimRef (PrimState m) Int
  -- ^ A primitive reference.
  -> Int
  -- ^ An 'Int' to AND with the current value of the primitive reference.
  -> m Int
  -- ^ A 'PrimMonad' action that atomically sets the value of the given
  --   primitive reference to the result of bitwise ANDing the given 'Int'
  --   with the primitive reference's current value, and then returns the value
  --   the primitive reference had before this modification.
fetchAndInt (MkPrimRef mba) (I# x) = primitive $ \s -> do
  let m = MByteArray.underlyingMutableByteArray# mba
  case GHC.Prim.fetchAndIntArray# m 0# x s of
    (# s', result #) -> (# s', I# result #)

-- | Given a reference and a value with which to bitwise NAND, atomically NAND
--   the value with the element.
--
--   Returns the value of the element before the operation.
--
--   Implies a full memory barrier.
fetchNandInt
  :: (PrimMonad m)
  => PrimRef (PrimState m) Int
  -- ^ A primitive reference.
  -> Int
  -- ^ An 'Int' to NAND with the current value of the primitive reference.
  -> m Int
  -- ^ A 'PrimMonad' action that atomically sets the value of the given
  --   primitive reference to the result of bitwise NANDing the given 'Int'
  --   with the primitive reference's current value, and then returns the value
  --   the primitive reference had before this modification.
fetchNandInt (MkPrimRef mba) (I# x) = primitive $ \s -> do
  let m = MByteArray.underlyingMutableByteArray# mba
  case GHC.Prim.fetchNandIntArray# m 0# x s of
    (# s', result #) -> (# s', I# result #)

-- | Given a reference and a value with which to bitwise OR, atomically OR
--   the value with the element.
--
--   Returns the value of the element before the operation.
--
--   Implies a full memory barrier.
fetchOrInt
  :: (PrimMonad m)
  => PrimRef (PrimState m) Int
  -- ^ A primitive reference.
  -> Int
  -- ^ An 'Int' to OR with the current value of the primitive reference.
  -> m Int
  -- ^ A 'PrimMonad' action that atomically sets the value of the given
  --   primitive reference to the result of bitwise ORing the given 'Int'
  --   with the primitive reference's current value, and then returns the value
  --   the primitive reference had before this modification.
fetchOrInt (MkPrimRef mba) (I# x) = primitive $ \s -> do
  let m = MByteArray.underlyingMutableByteArray# mba
  case GHC.Prim.fetchOrIntArray# m 0# x s of
    (# s', result #) -> (# s', I# result #)

-- | Given a reference, and a value with which to bitwise XOR, atomically XOR
--   the value with the element.
--
--   Returns the value of the element before the operation.
--
--   Implies a full memory barrier.
fetchXorInt
  :: (PrimMonad m)
  => PrimRef (PrimState m) Int
  -- ^ A primitive reference.
  -> Int
  -- ^ An 'Int' to XOR with the current value of the primitive reference.
  -> m Int
  -- ^ A 'PrimMonad' action that atomically sets the value of the given
  --   primitive reference to the result of bitwise XORing the given 'Int'
  --   with the primitive reference's current value, and then returns the value
  --   the primitive reference had before this modification.
fetchXorInt (MkPrimRef mba) (I# x) = primitive $ \s -> do
  let m = MByteArray.underlyingMutableByteArray# mba
  case GHC.Prim.fetchXorIntArray# m 0# x s of
    (# s', result #) -> (# s', I# result #)

--------------------------------------------------------------------------------

-- | FIXME: doc
unsafeToMByteArray
  :: PrimRef s a
  -- ^ FIXME: doc
  -> MByteArray s
  -- ^ FIXME: doc
unsafeToMByteArray (MkPrimRef mba) = mba
{-# INLINE unsafeToMByteArray #-}

-- | FIXME: doc
unsafeFromMByteArray
  :: MByteArray s
  -- ^ FIXME: doc
  -> PrimRef s a
  -- ^ FIXME: doc
unsafeFromMByteArray = MkPrimRef
{-# INLINE unsafeFromMByteArray #-}

--------------------------------------------------------------------------------
