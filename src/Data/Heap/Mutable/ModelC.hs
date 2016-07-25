{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides a variant of a mutable binary min heap that is used elsewhere to implement
--   Dijkstra\'s algorithm. It is unlikely that there are other uses for this specific
--   implementation. The binary heap in this module uses the standard array-as-binary-heap
--   approach where the 0-index item in the array is unused, the 1-index item is the
--   root, and the @n@ element has its left child at @2n@ and its right child at @2n + 1@.
--   The following additions (which are uncommon) have been made:
--
--   * This heap only supports 'Int' elements but is polymorphic in the priority data type.
--   * When the heap is initialized, it is given an 'Int'. This represents and exclusive upper bound
--     on the allowed elements. For example, if you pass 40, then you can only push 0 through 39 as
--     elements.
--   * Most of the functions in this module take an extra 'Int' (right after the 'RawHeap' argument).
--     This 'Int' tells us the number of items currently in the heap. In some cases, this argument
--     is not even used, but it is present so that LiquidHaskell can provide extra bound-checking assurances.
--     For example, if we initialize the heap with @new 90@, and then push three elements, we do not
--     want to be able to read the 83rd element in the 'rawHeapPriorities' and 'rawHeapElements'
--     arrays. Even though the this index is technically in bounds, the element stored there is not
--     actually in the heap. Many places where this bounding number is passed around to a function
--     should be eliminated by the inliner and do not affect runtime performance. The @ModelD@ module
--     provides a much more usable heap implementation where the currenty heap size is stored in a 'MutVar'.
--     It is not implemented as a 'MutVar' in here because LiquidHaskell cannot (to my knowledge)
--     use mutable values for meaningful proofs.
--   * This heap implements decrease-key as a part of 'push'. If you push an already existing element
--     onto the heap, the priority of the existing one and the priority of the one you are attempting
--     to push will be combined with the 'Monoid' instance. (Note: this could definitely be weakened
--     to 'Semigroup'). At the moment, only bubble up is attempted after this operation, so if this
--     causing the priority to increas, the heap becomes invalid (but not in a way that causes
--     segfaults).
--
--   As a result of the third constraint, the 'Monoid' instance and 'Ord' instance of the priority type
--   must obey these additional laws:
--
--   > mappend a b ≤ a
--   > mappend a b ≤ b
--   > mempty ≥ c
--
--   In more colloquial terms, the monoidal append of two priorities must be less than or equal
--   to the smaller of the two. Additionally, 'mempty' must be the largest priority.

module Data.Heap.Mutable.ModelC where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Primitive.Types (sizeOf#)
import GHC.Types (Int(..))
import Data.Vector (Vector,MVector)
import Data.Bits (unsafeShiftL,unsafeShiftR)
import Data.Word
import Data.Coerce
import Data.Vector.Unboxed (Unbox)
import Data.Primitive.Array
import Data.Primitive.ByteArray
import Debug.Trace
import qualified Data.Primitive.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU


{-@ type Positive = {n:Int | n > 0} @-}

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
  , rawHeapInvertedIndex :: !(MutableByteArray s) -- ^ Lookup binary tree index by element, used for increase and decrease priority
  }

{-@ assume readHeapPriority :: PrimMonad m
      => h:RawHeap (PrimState m) p -> bound:{bound:Nat | bound <= rawHeapBound h}
      -> ix:{v:Nat | v > 0 && v <= bound } -> m p @-}
readHeapPriority :: PrimMonad m => RawHeap (PrimState m) p -> Int -> Int -> m p
readHeapPriority (RawHeap _ priorities _ _) _ ix = readArray priorities ix

{-@ assume readHeapElement :: PrimMonad m => h:RawHeap (PrimState m) p
      -> bound:{bound:Nat | bound <= rawHeapBound h}
      -> ix:{v:Positive | v <= bound }
      -> m {e:Nat | e < rawHeapBound h} @-}
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
      -> ix:{v:Positive | v <= bound }
      -> e:{v:Nat | v < rawHeapBound h }
      -> m () @-}
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

{-@ assume writeHeapInvertedIndex :: PrimMonad m
      => h:RawHeap (PrimState m) p
      -> bound:{bound:Nat | bound <= rawHeapBound h }
      -> e:{v:Nat | v < rawHeapBound h }
      -> ix:{v:Nat | v <= bound }
      -> m ()
@-}
writeHeapInvertedIndex :: PrimMonad m => RawHeap (PrimState m) p -> Int -> Int -> Int -> m ()
writeHeapInvertedIndex (RawHeap _ _ _ invIndex) _ = writeByteArray invIndex

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
  c <- readHeapPriority h bound ix1
  d <- readHeapPriority h bound ix2
  writeHeapPriority h bound ix1 d
  writeHeapPriority h bound ix2 c
  writeHeapInvertedIndex h bound a ix2
  writeHeapInvertedIndex h bound b ix1

{-@ pop :: (PrimMonad m, Ord p)
      => h:RawHeap (PrimState m) p
      -> bound:{bound:Nat | bound <= rawHeapBound h}
      -> m ({k:Nat | if bound = 0 then k = 0 else k = bound - 1},Maybe (p,Int))
@-}
pop :: (PrimMonad m, Ord p) => RawHeap (PrimState m) p -> Int -> m (Int,Maybe (p,Int))
pop h currentSize = if currentSize > 0
  then do
    let newSize = currentSize - 1
    priority <- readHeapPriority h currentSize 1
    element <- readHeapElement h currentSize 1
    writeHeapInvertedIndex h currentSize element 0
    if (newSize > 0)
      then do
        lastPriority <- readHeapPriority h currentSize currentSize
        lastElement <- readHeapElement h currentSize currentSize
        writeHeapPriority h currentSize 1 lastPriority
        writeHeapElement h currentSize 1 lastElement
        writeHeapInvertedIndex h newSize lastElement 1
        bubbleDown h newSize
      else return ()
    return (newSize, Just (priority,element))
  else return (currentSize,Nothing)

{-@ bubbleDown :: (Ord p, PrimMonad m)
      => h:RawHeap (PrimState m) p
      -> bound:{bound:Positive | bound <= rawHeapBound h}
      -> m ()
@-}
bubbleDown :: forall p m. (Ord p, PrimMonad m)
  => RawHeap (PrimState m) p
  -> Int
  -> m ()
bubbleDown h currentSizeX = go currentSizeX 1 where
  {-@ go :: bnd:{bnd:Positive | bnd <= rawHeapBound h} -> ix:Positive -> m () / [bnd - ix] @-}
  go :: Int -> Int -> m ()
  go !currentSize !ix = do
    let leftChildIx = ix + ix
        rightChildIx = leftChildIx + 1
    if rightChildIx > currentSize
      then if leftChildIx == currentSize
        then do
          let childIx = leftChildIx
          myPriority <- readHeapPriority h currentSize ix
          childPriority <- readHeapPriority h currentSize childIx
          if childPriority < myPriority
            then do
              myElement <- readHeapElement h currentSize ix
              childElement <- readHeapElement h currentSize childIx
              swapHeap h currentSize ix childIx
              -- go childIx -- not needed here bc we know there will not be further children
            else return ()
        else return ()
      else do
        myPriority <- readHeapPriority h currentSize ix
        leftChildPriority <- readHeapPriority h currentSize leftChildIx
        rightChildPriority <- readHeapPriority h currentSize rightChildIx
        let (childIx,childPriority) = if leftChildPriority < rightChildPriority
              then (leftChildIx,leftChildPriority)
              else (rightChildIx,rightChildPriority)
        if childPriority < myPriority
          then do
            myElement <- readHeapElement h currentSize ix
            childElement <- readHeapElement h currentSize childIx
            swapHeap h currentSize ix childIx
            go currentSize childIx
          else return ()

{-@ unsafePush :: (Ord p, Monoid p, PrimMonad m)
               => p -> e:Nat -> oldSize:Nat
               -> {h:RawHeap (PrimState m) p | e < rawHeapBound h && oldSize <= rawHeapBound h}
               -> m {newSize:Nat | newSize > 0} @-}
unsafePush :: forall m p k. (Ord p, Monoid p, PrimMonad m)
  => p -> Int -> Int -> RawHeap (PrimState m) p -> m Int
unsafePush priority element currentSize h@(RawHeap bound _ _ _) = do
  existingElemIndex <- readHeapInvertedIndex h currentSize element
  if existingElemIndex == 0
    then if currentSize < rawHeapBound h
      then do
        let newSize = currentSize + 1
        appendElem priority element newSize h
        return newSize
      else error "unsafePush: This cannot ever happen (2)"
    else if currentSize > 0
      then do
        combineElem priority element currentSize existingElemIndex h
        return currentSize
      else error "unsafePush: This cannot ever happen (1)"

{-@ appendElem :: (Ord p, Monoid p, PrimMonad m)
               => p -> e:Nat -> newSize:{newSize:Nat|newSize > 0}
               -> {h:RawHeap (PrimState m) p | e < rawHeapBound h && newSize <= rawHeapBound h}
               -> m () @-}
appendElem :: (Ord p, PrimMonad m)
  => p -> Int -> Int -> RawHeap (PrimState m) p -> m ()
appendElem priority element currentSize h = do
  writeHeapPriority h currentSize currentSize priority
  writeHeapElement h currentSize currentSize element
  writeHeapInvertedIndex h currentSize element currentSize
  bubbleUp currentSize currentSize h

{-@ combineElem :: (Ord p, Monoid p, PrimMonad m)
                => p -> e:Nat -> sz:Positive -> ix:{ix:Positive | ix <= sz}
                -> {h:RawHeap (PrimState m) p | e < rawHeapBound h && sz <= rawHeapBound h}
                -> m () @-}
combineElem :: (Monoid p, Ord p, PrimMonad m)
  => p -> Int -> Int -> Int -> RawHeap (PrimState m) p -> m ()
combineElem priority element currentSize existingIndex h = do
  existingPriority <- readHeapPriority h currentSize existingIndex
  let newPriority = mappend priority existingPriority
  writeHeapPriority h currentSize existingIndex newPriority
  bubbleUp currentSize existingIndex h

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
        myPriority <- readHeapPriority h currentSize ix
        parentPriority <- readHeapPriority h currentSize parentIx
        if myPriority < parentPriority
          then do
            swapHeap h currentSize ix parentIx
            go parentIx
          else return ()
      else return ()

{-@ new :: (PrimMonad m, Monoid p) => bound:Nat
      -> m ({n:Int| n = 0},{h:RawHeap (PrimState m) p | rawHeapBound h = bound}) @-}
new :: (PrimMonad m, Monoid p) => Int -> m (Int,RawHeap (PrimState m) p)
new bound = do
  let boundPlusOne = bound + 1
  priorities <- newArray boundPlusOne mempty
  elements <- newByteArray (boundPlusOne * (I# (sizeOf# (undefined :: Int))))
  invertedIndex <- newByteArray (bound * (I# (sizeOf# (undefined :: Int))))
  setByteArray invertedIndex 0 bound (0 :: Int)
  return (0,RawHeap bound priorities elements invertedIndex)

