{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies  #-}

module Data.Heap.Mutable.ModelC where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Vector (Vector,MVector)
import Data.Bits (unsafeShiftL,unsafeShiftR)
import Data.Word
import Data.Coerce
import Data.Vector.Unboxed (Unbox)
import Data.Primitive.Array
import Data.Primitive.ByteArray
import qualified Data.Primitive.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU


{-@ type Positive = {n:Int | n > 0} @-}

{-  measure mvlen :: forall a. (MVector s a) -> Int @-}
{-  measure umvlen :: forall a. (MU.MVector s a) -> Int @-}
{-  assume MV.unsafeWrite :: PrimMonad m => x:(MVector (PrimState m) a) -> {i:Int | i < mvlen x } -> a -> m () @-}
{-  assume MU.unsafeWrite :: (Unbox a, PrimMonad m) => x:(MU.MVector (PrimState m) a) -> {i:Int | i < umvlen x } -> a -> m () @-}
{-  assume MV.length :: forall a. x:(Data.Vector.MVector s a) -> {v : Nat | v = vlen x } @-}

{-@ data RawHeap s p = RawHeap
      { rawHeapBound         :: Nat
      , rawHeapPriorities    :: (MutableArray s p)
      , rawHeapElements      :: (MutableByteArray s)
      , rawHeapInvertedIndex :: (MutableByteArray s)
      }
@-}
data RawHeap s p = RawHeap
  { rawHeapBound         :: !Int -- ^ This bound is exclusive
  , rawHeapPriorities    :: !(MutableArray s p) -- ^ Binary tree of priorities
  , rawHeapElements      :: !(MutableByteArray s) -- ^ Binary tree of elements
  , rawHeapInvertedIndex :: !(MutableByteArray s) -- ^ Lookup binary tree index by element
  }

{-@ assume readHeapPriority :: PrimMonad m
      => h:RawHeap (PrimState m) p -> bound:{bound:Nat | bound <= rawHeapBound h}
      -> ix:{v:Nat | v > 0 && v <= bound } -> m p @-}
readHeapPriority :: PrimMonad m => RawHeap (PrimState m) p -> Int -> Int -> m p
readHeapPriority (RawHeap _ priorities _ _) _ ix = readArray priorities ix

{-@ assume readHeapElement :: PrimMonad m => h:RawHeap (PrimState m) p
      -> bound:{bound:Nat | bound <= rawHeapBound h}
      -> ix:{v:Positive | v <= bound } -> m Int @-}
readHeapElement :: PrimMonad m => RawHeap (PrimState m) p -> Int -> Int -> m Int
readHeapElement (RawHeap _ _ elements _) _ ix = readByteArray elements ix

{-@ assume writeHeapPriority :: PrimMonad m
      => h:RawHeap (PrimState m) p -> bound:{bound:Nat | bound <= rawHeapBound h}
      -> ix:{v:Nat | v > 0 && v <= bound } -> p -> m () @-}
writeHeapPriority :: PrimMonad m => RawHeap (PrimState m) p -> Int -> Int -> p -> m ()
writeHeapPriority (RawHeap _ priorities _ _) _ = writeArray priorities

{-@ assume writeHeapElement :: PrimMonad m
      => h:RawHeap (PrimState m) p
      -> bound:{bound:Nat | bound <= rawHeapBound h}
      -> ix:{v:Positive | v <= bound } -> Int -> m () @-}
writeHeapElement :: PrimMonad m => RawHeap (PrimState m) p -> Int -> Int -> Int -> m ()
writeHeapElement (RawHeap _ _ elements _) _ = writeByteArray elements

{-@ assume readHeapInvertedIndex :: PrimMonad m
      => h:RawHeap (PrimState m) p
      -> bound:{bound:Nat | bound <= rawHeapBound h }
      -> ix:{v:Nat | v < rawHeapBound h }
      -> m {x:Positive|x < bound}
@-}
readHeapInvertedIndex :: PrimMonad m => RawHeap (PrimState m) p -> Int -> Int -> m Int
readHeapInvertedIndex (RawHeap _ _ _ invIndex) _ ix = readByteArray invIndex ix

{-@ swapHeap :: (Ord p, Monoid p, PrimMonad m)
      => h:RawHeap (PrimState m) p
      -> bound:{bound:Nat | bound <= rawHeapBound h}
      -> ix1:{ix1:Positive | ix1 <= bound}
      -> ix2:{ix2:Positive | ix2 <= bound}
      -> m () @-}
swapHeap :: PrimMonad m => RawHeap (PrimState m) p -> Int -> Int -> Int -> m ()
swapHeap h bound ix1 ix2 = do
  a <- readHeapElement h bound ix1
  b <- readHeapElement h bound ix2
  writeHeapElement h bound ix1 b
  writeHeapElement h bound ix2 a

{-@ unsafePush :: (Ord p, Monoid p, PrimMonad m)
               => p -> e:Nat -> oldSize:Nat
               -> {h:RawHeap (PrimState m) p | e < rawHeapBound h && oldSize <= rawHeapBound h}
               -> m {newSize:Nat | newSize > 0} @-}
unsafePush :: forall m p k. (Ord p, PrimMonad m)
  => p -> Int -> Int -> RawHeap (PrimState m) p -> m Int
unsafePush priority element currentSize h@(RawHeap bound _ _ _) = do
  existingElemIndex <- readHeapInvertedIndex h currentSize element
  if existingElemIndex == 0
    then if currentSize < rawHeapBound h
      then do
        let newSize = currentSize + 1
        appendElem priority element newSize h
        return newSize
      else error "unsafePush: This cannot ever happen"
    else if currentSize > 0
      then do
        combineElem priority element currentSize existingElemIndex h
        return currentSize
      else error "unsafePush: This cannot ever happen"

{-@ hey :: Monad m => n:Nat -> m {k:Nat | k > n} @-}
hey :: Monad m => Int -> m Int
hey a = return (a + 4)

{-@ appendElem :: (Ord p, Monoid p, PrimMonad m)
               => p -> e:Nat -> newSize:{newSize:Nat|newSize > 0}
               -> {h:RawHeap (PrimState m) p | e < rawHeapBound h && newSize <= rawHeapBound h}
               -> m () @-}
appendElem :: (Ord p, PrimMonad m)
  => p -> Int -> Int -> RawHeap (PrimState m) p -> m ()
appendElem priority element currentSize h = do
  writeHeapPriority h currentSize currentSize priority
  bubbleUp currentSize currentSize h

{-@ combineElem :: (Ord p, Monoid p, PrimMonad m)
                => p -> e:Nat -> sz:Positive -> ix:{ix:Positive | ix <= sz}
                -> {h:RawHeap (PrimState m) p | e < rawHeapBound h && sz <= rawHeapBound h}
                -> m () @-}
combineElem :: (Ord p, PrimMonad m)
  => p -> Int -> Int -> Int -> RawHeap (PrimState m) p -> m ()
combineElem priority element currentSize existingIndex h = do
  existingIndex

-- |
{-@ bubbleUp :: (Ord p, PrimMonad m)
             => sz:Positive
             -> ix:{ix:Positive|ix <= sz}
             -> {h:RawHeap (PrimState m) p | sz <= rawHeapBound h}
             -> m () @-}
bubbleUp :: (Ord p, PrimMonad m)
         => Int
         -> Int
         -> RawHeap (PrimState m) p
         -> m ()
bubbleUp currentSize startIx h = go startIx where
  go !ix = do
    let parentIx = ix `div` 2 -- getParentIndex ix, make this more efficient, use shifting
    if parentIx > 0
      then do
        myElement <- readHeapElement h currentSize ix
        parentElement <- readHeapElement h currentSize parentIx
        myPriority <- readHeapPriority h currentSize ix
        parentPriority <- readHeapPriority h currentSize parentIx
        if myPriority < parentPriority
          then do
            swapHeap h currentSize ix parentIx
            -- swapHeapPriority
            go parentIx
          else return ()
      else return ()

-- new :: (PrimMonad m, Monoid p) => Int -> m (RawHeap (PrimState m) p)
-- new bound = do
--   let boundPlusOne = bound + 1
--   priorities <- newArray boundPlusOne mempty
--   elements <- newByteArray boundPlusOne
--   invertedIndex <- newByteArray bound
--   return (RawHeap bound priorities elements invertedIndex)

