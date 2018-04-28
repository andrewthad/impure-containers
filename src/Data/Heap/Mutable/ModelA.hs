{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-@ LIQUID "--notermination" @-}
module Data.Heap.Mutable.ModelA where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits                    (unsafeShiftL, unsafeShiftR)
import           Data.Coerce
import           Data.Primitive.MutVar
import           Data.Vector                  (MVector, Vector)
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as MV
import qualified Data.Vector.Unboxed          as U
import qualified Data.Vector.Unboxed.Mutable  as MU
import           Data.Word

import           Data.Vector.Unboxed.Deriving

newtype Elem = Elem { getElem :: Int }

{-@ measure mvlen :: forall a. (MVector s a) -> Int @-}
{-@ assume MV.unsafeRead :: PrimMonad m => x:(MVector (PrimState m) a) -> vec:{v:Nat | v < mvlen x } -> m a @-}

{-@ measure elemVal :: Elem -> Int @-}
elemVal :: Elem -> Int
elemVal (Elem i) = i

derivingUnbox "Elem"
    [t| Elem -> Int |]
    [| coerce |]
    [| coerce |]

data Heap s p = Heap
  { heapBinaryTree      :: MVector s p -- ^ Binary tree of priorities
  , heapBinaryTreeElem  :: MU.MVector s Elem -- ^ Binary tree of elements
  , heapElemLookupIndex :: MU.MVector s Int -- ^ Lookup binary tree index by element
  , heapSize            :: MutVar s Int -- ^ Current heap size
  , heapMax             :: Int
  }

{-@ measure maxVal :: Heap s p -> Int @-}
maxVal :: Heap s p -> Int
maxVal (Heap _ _ _ _ h) = h

_NOT_FOUND :: Int
_NOT_FOUND = (-1)

pushList :: (Ord p, PrimMonad m)
  => [(p, Elem)] -> Heap (PrimState m) p -> m ()
pushList xs h = forM_ xs $ \(p,e) -> push p e h

toList :: (Ord p, PrimMonad m)
  => Heap (PrimState m) p -> m [(p,Elem)]
toList h = do
  m <- pop h
  case m of
    Nothing -> return []
    Just r -> (r:) <$> toList h

push :: (Ord p, PrimMonad m)
  => p -> Elem -> Heap (PrimState m) p -> m ()
push priority element h@(Heap binTree binTreeElem lookupIx size _) = do
  let len = MV.length binTree
  if fromIntegral len > getElem element -- remove fromIntegral later
    then unsafePush priority element h
    else error "Heap Model A: tried to push out of bounds element"

{-@ elem3 :: MVector (PrimState m) a -> m a @-}
elem3 :: PrimMonad m => MVector (PrimState m) a -> m a
elem3 v = MV.read v 3

-- | Does not perform a bounds check to see if the
--   element is allowed.
{- unsafePush :: p -> e:Elem -> h:Heap (PrimState m) p -> {i:m () | elemVal e < maxVal h} @-}
unsafePush :: (Ord p, PrimMonad m)
  => p -> Elem -> Heap (PrimState m) p -> m ()
unsafePush priority element (Heap binTree binTreeElem lookupIx size _) = do
  currentSize <- readMutVar size
  let newSize = currentSize + 1
  writeMutVar size newSize
  MV.unsafeWrite binTree newSize priority
  MU.unsafeWrite binTreeElem newSize element
  bubbleUp newSize binTree binTreeElem lookupIx

pop :: (Ord p, PrimMonad m) => Heap (PrimState m) p -> m (Maybe (p,Elem))
pop (Heap binTree binTreeElem lookupIx size _) = do
  currentSize <- readMutVar size
  if currentSize > 0
    then do
      let newSize = currentSize - 1
      writeMutVar size newSize
      priority <- MV.unsafeRead binTree 1
      element <- MU.unsafeRead binTreeElem 1
      when (newSize > 0) $ do
        lastPriority <- MV.unsafeRead binTree currentSize
        lastElem <- MU.unsafeRead binTreeElem currentSize
        MV.unsafeWrite binTree 1 lastPriority
        MU.unsafeWrite binTreeElem 1 lastElem
        bubbleDown newSize binTree binTreeElem lookupIx
      return (Just (priority,element))
    else return Nothing

getChildrenIndices :: Int -> Int -> Children
getChildrenIndices currentSize i =
  let leftChild = unsafeShiftL i 1
      rightChild = leftChild + 1
   in if rightChild > currentSize
        then if leftChild > currentSize
          then NoChild
          else LeftChild leftChild
        else LeftRightChild leftChild rightChild

getParentIndex :: Int -> Int
getParentIndex i = unsafeShiftR i 1

bubbleUp :: (Ord p, PrimMonad m)
         => Int
         -> MVector (PrimState m) p
         -> MU.MVector (PrimState m) Elem
         -> MU.MVector (PrimState m) Int
         -> m ()
bubbleUp currentSize binTree binTreeElem lookupIx = go currentSize where
  go !ix = do
    let parentIx = getParentIndex ix
    if parentIx > 0
      then do
        myElement <- MU.unsafeRead binTreeElem ix
        parentElement <- MU.unsafeRead binTreeElem parentIx
        myPriority <- MV.unsafeRead binTree ix
        parentPriority <- MV.unsafeRead binTree parentIx
        if myPriority < parentPriority
          then do
            swapTwo binTree binTreeElem lookupIx myElement parentElement ix parentIx
            go parentIx
          else return ()
      else return ()

-- | The currentSize must be at least one
bubbleDown :: (Ord p, PrimMonad m)
  => Int -> MVector (PrimState m) p -> MU.MVector (PrimState m) Elem
  -> MU.MVector (PrimState m) Int
  -> m ()
bubbleDown currentSize binTree binTreeElem lookupIx = go 1 where
  go !ix = do
    case getChildrenIndices currentSize ix of
      NoChild -> return ()
      LeftChild leftChildIx -> do
        myPriority <- MV.unsafeRead binTree ix
        let childIx = leftChildIx
        childPriority <- MV.unsafeRead binTree childIx
        if childPriority < myPriority
          then do
            myElement <- MU.unsafeRead binTreeElem ix
            childElement <- MU.unsafeRead binTreeElem childIx
            swapTwo binTree binTreeElem lookupIx myElement childElement ix childIx
            go childIx
          else return ()
      LeftRightChild leftChildIx rightChildIx -> do
        myPriority <- MV.unsafeRead binTree ix
        leftChild <- MV.unsafeRead binTree leftChildIx
        rightChild <- MV.unsafeRead binTree rightChildIx
        let (childIx,childPriority) = if leftChild < rightChild
              then (leftChildIx,leftChild)
              else (rightChildIx,rightChild)
        if childPriority < myPriority
          then do
            myElement <- MU.unsafeRead binTreeElem ix
            childElement <- MU.unsafeRead binTreeElem childIx
            swapTwo binTree binTreeElem lookupIx myElement childElement ix childIx
            go childIx
          else return ()

swapTwo :: PrimMonad m
  => MVector (PrimState m) p
  -> MU.MVector (PrimState m) Elem
  -> MU.MVector (PrimState m) Int
  -> Elem
  -> Elem
  -> Int
  -> Int
  -> m ()
swapTwo binTree binTreeElem lookupIx a b i j = do
  MV.unsafeSwap binTree i j
  MU.unsafeSwap binTreeElem i j
  MU.unsafeSwap lookupIx (coerce a) (coerce b)


data Children
  = NoChild
  | LeftChild Int
  | LeftRightChild Int Int

-- push :: (Ord p, Monoid p) => Int -> p -> Heap (PrimState m) p -> m ()
-- push element priority (Heap binTree binTreeElem lookupIx size) = do

new :: PrimMonad m => Int -> m (Heap (PrimState m) p)
new elemCount = do
  let elemCountPlusOne = elemCount + 1
  binTree <- MV.new elemCountPlusOne
  binTreeElem <- MU.new elemCountPlusOne
  lookupIx <- MU.new elemCount
  MU.set lookupIx _NOT_FOUND
  size <- newMutVar 0
  return (Heap binTree binTreeElem lookupIx size elemCount)

