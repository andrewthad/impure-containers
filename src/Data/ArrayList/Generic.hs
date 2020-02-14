{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

{- |

Auto-growing 'ArrayList' type that over-allocates memory to optimize frequent resizes.

This is similar to @std::vector@ from C++ or @list@ from Python.

When an @ArrayList@ is requested to grow and current allocated capacity can't hold the
required number of items, capacity is multiplied by a constant factor of @1.5@, instead
of allocating just the required number of items. This ensures that frequent grow requests
will result in a much lower number of actual allocations and memory moves.

All functions in this module are marked @INLINEABLE@, so that they may be specialized to concrete
monad and vector types at the use-site.
-}
module Data.ArrayList.Generic
  (
    -- * Creation of @ArrayList@s
    ArrayList
  , new, fromVector

    -- * Accessing the underlying vector
  , vector, size
  , unsafeVector

    -- * Extending the @ArrayList@
  , push, grow

    -- * Deconstruction of @ArrayList@s
  , freeze
  ) where

import           Control.Monad               (when)
import           Control.Monad.Primitive
import           Control.Monad.ST            (ST)
import           Data.Primitive.MutVar
import           Data.Vector.Generic         (Mutable, Vector)
import qualified Data.Vector.Generic         as GV
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed.Mutable as VUM
import           GHC.Prim                    (RealWorld)

data ArrayList v s a = ArrayList
  { arrayListSize   :: !(MutVar s Int)
  , arrayListVector :: !(MutVar s (v s a))
  }

-- | Create a new 'ArrayList' with requested capacity. The length of 'ArrayList' as reported by
-- 'size' will be @0@.
new :: (PrimMonad m, MVector v a) => Int -> m (ArrayList v (PrimState m) a)
new len = ArrayList <$> newMutVar 0 <*> (newMutVar =<< GM.new len)
{-# INLINEABLE new #-}

-- | Create a new 'ArrayList' from immutable vector. Capacity will be equal to the length of vector.
-- Memory will be copied, so that input vector is safe to use afterwards.
fromVector :: (PrimMonad m, Vector v a) => v a -> m (ArrayList (Mutable v) (PrimState m) a)
fromVector vec = ArrayList <$> newMutVar (GV.length vec) <*> (newMutVar =<< GV.thaw vec)
{-# INLINEABLE fromVector #-}

-- | Get underlying mutable vector that stores @ArrayList@'s data. Length of this vector will be set
-- to the actually used length of @ArrayList@, not its capacity.
vector :: (PrimMonad m, MVector v a) => ArrayList v (PrimState m) a -> m (v (PrimState m) a)
vector (ArrayList sizeRef mvecRef) = do
  !size_ <- readMutVar sizeRef
  GM.unsafeTake size_ <$> readMutVar mvecRef
{-# INLINEABLE vector #-}

-- | Like 'vector', but do not set the length of resulting vector. Its length will be equal to the
-- capacity of 'ArrayList'. This means that some elements of the vector will be uninitialized.
unsafeVector :: (PrimMonad m, MVector v a) => ArrayList v (PrimState m) a -> m (v (PrimState m) a)
unsafeVector (ArrayList _ mvecRef) = readMutVar mvecRef
{-# INLINEABLE unsafeVector #-}

-- | Get currently used size of the 'ArrayList'.
size :: (PrimMonad m, MVector v a) => ArrayList v (PrimState m) a -> m Int
size (ArrayList sizeRef _) = readMutVar sizeRef
{-# INLINEABLE size #-}

-- | Append an element to the end of the 'ArrayList'.
push :: (PrimMonad m, MVector v a) => ArrayList v (PrimState m) a -> a -> m ()
push al@(ArrayList sizeRef mvecRef) a = do
  grow al 1

  !size_ <- readMutVar sizeRef
  !mvec <- readMutVar mvecRef

  GM.unsafeWrite mvec (size_ - 1) a
{-# INLINEABLE push #-}
{-# SPECIALIZE push :: ArrayList VM.MVector RealWorld a -> a -> IO () #-}
{-# SPECIALIZE push :: ArrayList VM.MVector s a -> a -> ST s () #-}
{-# SPECIALIZE push :: (GM.MVector VUM.MVector a) => ArrayList VUM.MVector RealWorld a -> a -> IO () #-}
{-# SPECIALIZE push :: (GM.MVector VUM.MVector a) => ArrayList VUM.MVector s a -> a -> ST s () #-}
{-# SPECIALIZE push :: (MVector v a) => ArrayList v RealWorld a -> a -> IO () #-}
{-# SPECIALIZE push :: (MVector v a) => ArrayList v s a -> a -> ST s () #-}

-- | Request to add more items to the vector. Used size will be set to current size + extra requested size.
--
-- __NOTE__: Do not use the result of 'vector' after calling this function, as it may refer to discarded
-- memory.
--
-- This function defines two Cost Centres: "@ArrayList.grow.toNewSize@" when vector will grow to fit
-- precisely the number of required elements, and "@ArrayList.grow.toNewCapacity@" when vector will
-- over-allocate memory.
--
-- These may be useful when profiling your program to check how much allocations and moves 'ArrayList'
-- does actually save.
--
-- This function is specialized to 'IO' and 'ST' monads, as well to simple 'VM.MVector' and unboxed
-- 'VUM.MVector' types.
grow :: (PrimMonad m, MVector v a) => ArrayList v (PrimState m) a -> Int -> m ()
grow (ArrayList sizeRef mvecRef) extraSize = do
  !size_ <- readMutVar sizeRef
  !mvec <- readMutVar mvecRef

  let
    capacity = GM.length mvec
    newSize = size_ + extraSize
    -- Next step in exponential grow
    newCapacity = floor $ fromIntegral capacity * factor

  -- Need to grow the vector only if the current capacity can't hold the requested size
  when (capacity < newSize) $ do
      -- Grow vector to be max (capacity * factor) (curSize + extraSize)
      -- unsafeGrow will reallocate the vector and return newly created one.
      --
      -- NOTE: unsafeGrow accepts extra size requested, not total new size.
      !mvec' <- if newSize > newCapacity
         then
           {-# SCC "ArrayList.grow.toNewSize" #-} GM.unsafeGrow mvec (newSize - capacity)
         else
           {-# SCC "ArrayList.grow.toNewCapacity" #-} GM.unsafeGrow mvec (newCapacity - capacity)

      -- Ensure that the memory requested by user is initialized.
      --GM.basicInitialize $ GM.unsafeSlice size_ extraSize mvec'

      writeMutVar mvecRef mvec'

  -- In any case, remeber current used size.
  writeMutVar sizeRef newSize
{-# INLINEABLE grow #-}
{-# SPECIALIZE grow :: ArrayList VM.MVector RealWorld a -> Int -> IO () #-}
{-# SPECIALIZE grow :: ArrayList VM.MVector s a -> Int -> ST s () #-}
{-# SPECIALIZE grow :: (GM.MVector VUM.MVector a) => ArrayList VUM.MVector RealWorld a -> Int -> IO () #-}
{-# SPECIALIZE grow :: (GM.MVector VUM.MVector a) => ArrayList VUM.MVector s a -> Int -> ST s () #-}
{-# SPECIALIZE grow :: (MVector v a) => ArrayList v RealWorld a -> Int -> IO () #-}
{-# SPECIALIZE grow :: (MVector v a) => ArrayList v s a -> Int -> ST s () #-}

factor :: Double
factor = 1.5

-- | Make a copy of currently used part of 'ArrayList' and return it as an immutable vector.
freeze :: (PrimMonad m, Vector v a) => ArrayList (Mutable v) (PrimState m) a -> m (v a)
freeze (ArrayList sizeRef mvecRef) = do
  !size_ <- readMutVar sizeRef
  !mvec <- readMutVar mvecRef
  let sizedMVec = GM.unsafeTake size_ mvec
  GV.freeze sizedMVec
{-# INLINEABLE freeze #-}
