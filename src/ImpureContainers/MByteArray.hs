--------------------------------------------------------------------------------

{-# LANGUAGE MagicHash #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module ImpureContainers.MByteArray
  ( -- * 'MByteArray'
    MByteArray
  , underlyingMutableByteArray#

    -- ** Creation
  , new, newPinned, newAlignedPinned

    -- ** Element access
  , read, write

    -- ** Freezing and thawing
  , unsafeFreeze, unsafeThaw

    -- ** Block operations
  , copyFromIByteArray
  , copyFromMByteArray
  , move
  , set
  , fill

    -- ** Information
  , sizeof
  , same
  , contents
  ) where

--------------------------------------------------------------------------------

import           Prelude                 ()

import           Data.Bool               (Bool)
import           Data.Int                (Int)
import           Data.Word               (Word8)

import           Control.Monad.Primitive
import           Data.Coerce             (coerce)
import           Data.Primitive          (Addr, Prim)
import qualified Data.Primitive

import qualified GHC.Prim

--------------------------------------------------------------------------------

-- | FIXME: doc
type MByteArray s = Data.Primitive.MutableByteArray s

-- | FIXME: doc
type IByteArray = Data.Primitive.ByteArray

--------------------------------------------------------------------------------

-- | FIXME: doc
underlyingMutableByteArray#
  :: MByteArray s
  -- ^ FIXME: doc
  -> GHC.Prim.MutableByteArray# s
  -- ^ FIXME: doc
underlyingMutableByteArray# (Data.Primitive.MutableByteArray mba) = mba
{-# INLINE underlyingMutableByteArray# #-}

--------------------------------------------------------------------------------

-- | Create a new mutable byte array of the specified size in bytes.
new
  :: (PrimMonad m)
  => Int
  -- ^ FIXME: doc
  -> m (MByteArray (PrimState m))
  -- ^ FIXME: doc
new = Data.Primitive.newByteArray
{-# INLINE new #-}

-- | Create a /pinned/ byte array of the specified size in bytes.
--   The garbage collector is guaranteed not to move it.
newPinned
  :: (PrimMonad m)
  => Int
  -- ^ FIXME: doc
  -> m (MByteArray (PrimState m))
  -- ^ FIXME: doc
newPinned = Data.Primitive.newPinnedByteArray
{-# INLINE newPinned #-}

-- | Create a /pinned/ byte array of the specified size in bytes and with the
--   give alignment. The garbage collector is guaranteed not to move it.
newAlignedPinned
  :: (PrimMonad m)
  => Int
  -- ^ FIXME: doc
  -> Int
  -- ^ FIXME: doc
  -> m (MByteArray (PrimState m))
  -- ^ FIXME: doc
newAlignedPinned = Data.Primitive.newAlignedPinnedByteArray
{-# INLINE newAlignedPinned #-}

-- | Yield a pointer to the array's data.
--   This operation is only safe on /pinned/ byte arrays allocated by
--   'newPinnedByteArray' or 'newAlignedPinnedByteArray'.
contents
  :: MByteArray s
  -- ^ FIXME: doc
  -> Addr
  -- ^ FIXME: doc
contents = Data.Primitive.mutableByteArrayContents
{-# INLINE contents #-}

-- | Check if the two arrays refer to the same memory block.
same
  :: MByteArray s
  -- ^ FIXME: doc
  -> MByteArray s
  -- ^ FIXME: doc
  -> Bool
  -- ^ FIXME: doc
same = Data.Primitive.sameMutableByteArray
{-# INLINE same #-}

-- | Convert a mutable byte array to an immutable one without copying.
--   The array should not be modified after the conversion.
unsafeFreeze
  :: (PrimMonad m)
  => MByteArray (PrimState m)
  -- ^ FIXME: doc
  -> m IByteArray
  -- ^ FIXME: doc
unsafeFreeze = Data.Primitive.unsafeFreezeByteArray
{-# INLINE unsafeFreeze #-}

-- | Convert an immutable byte array to a mutable one without copying. The
-- original array should not be used after the conversion.
unsafeThaw
  :: (PrimMonad m)
  => IByteArray
  -- ^ FIXME: doc
  -> m (MByteArray (PrimState m))
  -- ^ FIXME: doc
unsafeThaw = Data.Primitive.unsafeThawByteArray
{-# INLINE unsafeThaw #-}

-- | Size of the mutable byte array in bytes.
sizeof
  :: MByteArray s
  -- ^ FIXME: doc
  -> Int
  -- ^ FIXME: doc
sizeof = Data.Primitive.sizeofMutableByteArray
{-# INLINE sizeof #-}

-- | Read a primitive value from the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
read
  :: (Prim a, PrimMonad m)
  => MByteArray (PrimState m)
  -- ^ FIXME: doc
  -> Int
  -- ^ FIXME: doc
  -> m a
  -- ^ FIXME: doc
read = Data.Primitive.readByteArray
{-# INLINE read #-}

-- | Write a primitive value to the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
write
  :: (Prim a, PrimMonad m)
  => MByteArray (PrimState m)
  -- ^ FIXME: doc
  -> Int
  -- ^ FIXME: doc
  -> a
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
write = Data.Primitive.writeByteArray
{-# INLINE write #-}

-- | Copy a slice of an immutable byte array to a mutable byte array.
copyFromIByteArray
  :: (PrimMonad m)
  => MByteArray (PrimState m)
  -- ^ [FIXME: doc] destination array
  -> Int
  -- ^ [FIXME: doc] offset into destination array
  -> IByteArray
  -- ^ [FIXME: doc] source array
  -> Int
  -- ^ [FIXME: doc] offset into source array
  -> Int
  -- ^ [FIXME: doc] number of bytes to copy
  -> m ()
  -- ^ FIXME: doc
copyFromIByteArray = Data.Primitive.copyByteArray
{-# INLINE copyFromIByteArray #-}

-- | Copy a slice of a mutable byte array into another array.
--   The two slices must not overlap.
copyFromMByteArray
  :: (PrimMonad m)
  => MByteArray (PrimState m)
  -- ^ [FIXME: doc] destination array
  -> Int
  -- ^ [FIXME: doc] offset into destination array
  -> MByteArray (PrimState m)
  -- ^ [FIXME: doc] source array
  -> Int
  -- ^ [FIXME: doc] offset into source array
  -> Int
  -- ^ [FIXME: doc] number of bytes to copy
  -> m ()
  -- ^ FIXME: doc
copyFromMByteArray = Data.Primitive.copyMutableByteArray
{-# INLINE copyFromMByteArray #-}

-- | Copy a slice of a mutable byte array into another array.
--   The given arrays are allowed to overlap.
move
  :: (PrimMonad m)
  => MByteArray (PrimState m)
  -- ^ [FIXME: doc] destination array
  -> Int
  -- ^ [FIXME: doc] offset into destination array
  -> MByteArray (PrimState m)
  -- ^ [FIXME: doc] source array
  -> Int
  -- ^ [FIXME: doc] offset into source array
  -> Int
  -- ^ [FIXME: doc] number of bytes to copy
  -> m ()
  -- ^ FIXME: doc
move = Data.Primitive.moveByteArray
{-# INLINE move #-}

-- | Fill a slice of a mutable byte array with a value.
--
--   The offset and length parameters are given as a number of elements
--   of type @a@ rather than in bytes. In other words, the offset and length
--   work like C arrays, rather than like C pointers.
set
  :: (Prim a, PrimMonad m)
  => MByteArray (PrimState m)
  -- ^ [FIXME: doc] array to fill
  -> Int
  -- ^ [FIXME: doc] offset into array
  -> Int
  -- ^ [FIXME: doc] number of values to fill
  -> a
  -- ^ [FIXME: doc] value to fill with
  -> m ()
  -- ^ FIXME: doc
set = Data.Primitive.setByteArray
{-# INLINE set #-}

-- | Fill a slice of a mutable byte array with a byte.
fill
  :: (PrimMonad m)
  => MByteArray (PrimState m)
  -- ^ [FIXME: doc] array to fill
  -> Int
  -- ^ [FIXME: doc] offset into array
  -> Int
  -- ^ [FIXME: doc] number of bytes to fill
  -> Word8
  -- ^ [FIXME: doc] byte to fill with
  -> m ()
  -- ^ FIXME: doc
fill = Data.Primitive.fillByteArray
{-# INLINE fill #-}

--------------------------------------------------------------------------------
