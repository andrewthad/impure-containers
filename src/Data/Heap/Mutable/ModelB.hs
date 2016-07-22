{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies  #-}

module Data.Heap.Mutable.ModelB where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Vector (Vector,MVector)
import Data.Bits (unsafeShiftL,unsafeShiftR)
import Data.Word
import Data.Coerce
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Data.Vector.Unboxed.Deriving

newtype Elem = Elem { getElem :: Int }

{-@ measure mvlen :: forall a. (MVector s a) -> Int @-}
{-@ measure umvlen :: forall a. (MU.MVector s a) -> Int @-}
{-@ assume MV.unsafeRead :: PrimMonad m => x:(MVector (PrimState m) a) -> vec:{v:Nat | v < mvlen x } -> m a @-}
{-@ assume MV.unsafeWrite :: PrimMonad m => x:(MVector (PrimState m) a) -> {i:Int | i < mvlen x } -> a -> m () @-}
{-@ assume MU.unsafeWrite :: (Unbox a, PrimMonad m) => x:(MU.MVector (PrimState m) a) -> {i:Int | i < umvlen x } -> a -> m () @-}
{-@ assume MV.length :: forall a. x:(Data.Vector.MVector s a) -> {v : Nat | v = vlen x } @-}

{-  assume myReadMutVar :: PrimMonad m => x:(MutVar (PrimState m) a) -> {k:m a| v = vlen x } @-}
myReadMutVar :: PrimMonad m => MutVar (PrimState m) a -> m a
myReadMutVar = readMutVar

{-@ measure elemVal :: Elem -> Int @-}
elemVal :: Elem -> Int
elemVal (Elem i) = i

derivingUnbox "Elem"
  [t| Elem -> Int |]
  [| coerce |]
  [| coerce |]


{-@ data Heap s p = Heap { heapMax :: {m:Int | m >= 0}
                         , heapBinaryTree :: {mv : MVector s p | mvlen mv == heapMax}
                         , heapBinaryTreeElem :: {mv : MU.MVector s Elem | mvlen mv == heapMax}
                         , heapElemLookupIndex :: MU.MVector s Int
                         }
@-}

{-@ type HeapSized s p n = { h : Heap s p | mvlen (heapBinaryTree h) - 1 > n && umvlen (heapBinaryTreeElem h) - 1 > n} @-}
{-@ type HeapSizedX s p n = { h : Heap s p | mvlen (heapBinaryTree h) > n && umvlen (heapBinaryTreeElem h) > n} @-}

{-@ predicate Sized H N = ((mvlen (heapBinaryTree H)) > N && (umvlen (heapBinaryTreeElem H)) > N) @-}

data Heap s p = Heap
  { heapMax :: !Int
  , heapBinaryTree :: !(MVector s p) -- ^ Binary tree of priorities
  , heapBinaryTreeElem :: !(MU.MVector s Elem) -- ^ Binary tree of elements
  , heapElemLookupIndex :: !(MU.MVector s Int) -- ^ Lookup binary tree index by element
  }

{-@ measure maxVal :: Heap s p -> Int @-}
maxVal :: Heap s p -> Int
maxVal (Heap h _ _ _) = h

{-@ elem3 :: {mv:MVector (PrimState m) a | mvlen mv > 3} -> m a @-}
elem3 :: PrimMonad m => MVector (PrimState m) a -> m a
elem3 v = MV.unsafeRead v 3

-- | Does not perform a bounds check to see if the
--   element is allowed.
{-@ unsafePush :: p -> e:Elem -> oldSize:Int
               -> {h:Heap (PrimState m) p | elemVal e < maxVal h && Sized h (oldSize + 1)}
               -> m {newSize:Int | newSize == oldSize + 1} @-}
unsafePush :: forall m p k. (Ord p, PrimMonad m)
  => p -> Elem -> Int -> Heap (PrimState m) p -> m Int
unsafePush priority element currentSize (Heap _ binTree binTreeElem lookupIx) = do
  let newSize = currentSize + 1
  MV.unsafeWrite binTree newSize priority
  MU.unsafeWrite binTreeElem newSize element
  -- bubbleUp
  return newSize
  -- die "foobar" -- bubbleUp newSize binTree binTreeElem lookupIx

{-  bubbleUp :: currentSize:Int
             -> {binTree:MVector (PrimState m) p | mvlen binTree >
             -> binTreeElem:MVector (PrimState m) Elem
             -> lookupIx:MVector (PrimState m) Int
             -> m ()
@-}
-- bubbleUp :: (Ord p, PrimMonad m)
--          => Int
--          -> MVector (PrimState m) p
--          -> MU.MVector (PrimState m) Elem
--          -> MU.MVector (PrimState m) Int
--          -> m ()
-- bubbleUp currentSize binTree binTreeElem lookupIx = go currentSize where
--   go !ix = do
--     let parentIx = getParentIndex ix
--     if parentIx > 0
--       then do
--         myElement <- MU.unsafeRead binTreeElem ix
--         parentElement <- MU.unsafeRead binTreeElem parentIx
--         myPriority <- MV.unsafeRead binTree ix
--         parentPriority <- MV.unsafeRead binTree parentIx
--         if myPriority < parentPriority
--           then do
--             swapTwo binTree binTreeElem lookupIx myElement parentElement ix parentIx
--             go parentIx
--           else return ()
--       else return ()

{-@ die :: {v:String | false} -> a  @-}
die :: String -> a
die msg = error msg

