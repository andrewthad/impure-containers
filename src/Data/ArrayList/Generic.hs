{-# LANGUAGE BangPatterns #-}

module Data.ArrayList.Generic where

import           Control.Monad.Primitive
import           Data.Primitive.MutVar
import           Data.Vector.Generic         (Mutable, Vector)
import qualified Data.Vector.Generic         as GV
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as GM

data ArrayList v s a = ArrayList
  { arrayListSize   :: !(MutVar s Int)
  , arrayListVector :: !(MutVar s (v s a))
  }

new :: (PrimMonad m, MVector v a) => Int -> m (ArrayList v (PrimState m) a)
new len = ArrayList <$> newMutVar 0 <*> (newMutVar =<< GM.new len)

-- | Append an element to the end of the 'ArrayList'.
push :: (PrimMonad m, MVector v a) => ArrayList v (PrimState m) a -> a -> m ()
push (ArrayList sizeRef mvecRef) a = do
  !size <- readMutVar sizeRef
  !mvec <- readMutVar mvecRef
  let !newSize = size + 1
      vlen = GM.length mvec
  writeMutVar sizeRef newSize
  if size < vlen
    then GM.unsafeWrite mvec size a
    else do
      newMVec <- GM.unsafeGrow mvec size
      GM.unsafeWrite newMVec size a
      writeMutVar mvecRef newMVec

freeze :: (PrimMonad m, Vector v a) => ArrayList (Mutable v) (PrimState m) a -> m (v a)
freeze (ArrayList sizeRef mvecRef) = do
  !size <- readMutVar sizeRef
  !mvec <- readMutVar mvecRef
  let sizedMVec = GM.unsafeTake size mvec
  GV.freeze sizedMVec

