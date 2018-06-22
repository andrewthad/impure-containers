module Data.Heap.Mutable.ModelD where

import           Control.Monad
import           Control.Monad.Primitive
import qualified Data.Heap.Mutable.ModelC as I
import           Data.Primitive.MutVar
import           Debug.Trace

data Heap s p = Heap
  { heapRaw         :: !(I.RawHeap s p)
  , heapCurrentSize :: !(MutVar s Int)
  }

new :: (PrimMonad m, Monoid p)
  => Int -- ^ Maximum element
  -> m (Heap (PrimState m) p)
new i = if i < 0
  then error "mutable heap new: size must be positive"
  else do
    (sz,h) <- I.new i
    currentSize <- newMutVar 0
    return (Heap h currentSize)

-- | Does not check to see if the provided element is within
--   the bounds accepted by the 'Heap'.
unsafePush :: (Ord p, Monoid p, PrimMonad m)
  => p -- ^ Priority
  -> Int -- ^ Element
  -> Heap (PrimState m) p -- ^ Heap
  -> m ()
unsafePush priority element (Heap raw currentSize) = do
  oldSize <- readMutVar currentSize
  newSize <- I.unsafePush priority element oldSize raw
  writeMutVar currentSize newSize

push :: (Ord p, Monoid p, PrimMonad m)
  => p -- ^ Priority
  -> Int -- ^ Element
  -> Heap (PrimState m) p -- ^ Heap
  -> m ()
push priority element h@(Heap raw _) = if element < I.rawHeapBound raw
  then unsafePush priority element h
  else error "mutable heap push: element too big"

pushList :: (Ord p, PrimMonad m, Monoid p) => [(p, Int)] -> Heap (PrimState m) p -> m ()
pushList xs h = forM_ xs $ \(p,e) -> push p e h

popAll :: (Ord p, PrimMonad m) => Heap (PrimState m) p -> m [(p,Int)]
popAll h = do
  m <- pop h
  case m of
    Nothing -> return []
    Just r -> (r:) <$> popAll h

pop :: (PrimMonad m, Ord p) => Heap (PrimState m) p -> m (Maybe (p,Int))
pop (Heap raw currentSize) = do
  oldSize <- readMutVar currentSize
  (newSize,m) <- I.pop raw oldSize
  writeMutVar currentSize newSize
  return m


