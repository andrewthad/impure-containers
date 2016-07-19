{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Graph.Immutable.Tagged where

import Control.Monad.Primitive
import Data.Vector (Vector)
import Data.Vector.Mutable (MVector)
import Control.Monad
import Data.Word
import Control.Monad.ST (runST)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

data Vertex g = Vertex { getVertex :: Int }
data Edge g = Edge
  { edgeVertexA :: !Int
  , edgeVertexB :: !Int
  }

-- | The neighbor vertices and neighbor edges must have
--   equal length.
data Graph g e v = Graph
  { graphVertices :: Vector v
  , graphOutboundNeighborVertices :: Vector (U.Vector Int)
  , graphOutboundNeighborEdges :: Vector (Vector e)
  , graphEdges :: Int -> Int -> Maybe e
  } deriving (Functor)

-- instance Functor (Graph g e) where
--   fmap f g = g { graphVertices = Vector.map (graphVertices g) }

-- visited,allowed,notAllowed :: Word8
-- visited = 2
-- allowed = 1
-- notAllowed = 0

breadthFirstBy :: (Ord s, Monoid s)
               => (v -> v -> e -> s)
               -> Vertex g
               -> Graph g e v
               -> Graph g e s
breadthFirstBy f (Vertex v0) g@(Graph vertices outNeighbors outEdges _edges) = runST $ do
  let vertexCount = V.length vertices
  newVertices <- MV.new vertexCount
  MV.set newVertices mempty
  visited <- MU.new vertexCount
  MU.set visited False
  heap <- minHeapNew vertexCount
  minHeapInsert v0 mempty heap
  let keepGoing = do
        m <- minHeapPop heap
        case m of
          Nothing -> return ()
          Just (vertexIx,s) -> do
            MU.write visited vertexIx True
            MV.write newVertices vertexIx s
            let neighborVertices = outNeighbors V.! vertexIx
                neighborEdges = outEdges V.! vertexIx
                v1 = vertices V.! vertexIx
                runInsert neighborIx neighborVertexIx = do
                  let edgeVal = neighborEdges V.! neighborIx
                      v2 = vertices V.! neighborVertexIx
                  alreadyVisited <- MU.read visited neighborVertexIx
                  if alreadyVisited
                    then return ()
                    else minHeapInsert neighborVertexIx (f v1 v2 edgeVal) heap
            U.imapM_ runInsert neighborVertices
            keepGoing
  keepGoing
  newVerticesFrozen <- V.freeze newVertices
  return (g {graphVertices = newVerticesFrozen})

data Heap s p = Heap
  { heapBinaryTree :: MVector s p
  , heapElemLookupIndex :: MU.MVector s Int
  }

minHeapPop :: Ord p => Heap (PrimState m) p -> m (Maybe (Int,p))
minHeapPop _ = error "write me"

-- This may update an existing element
minHeapInsert :: (Ord p, Monoid p) => Int -> p -> Heap (PrimState m) p -> m ()
minHeapInsert _ _ = error "write me"

minHeapNew :: Int -> m (Heap (PrimState m) p)
minHeapNew _maxElement = error "write me"

lookupVertex :: Eq v => v -> Graph g e v -> Maybe (Vertex g)
lookupVertex val g = fmap Vertex (V.elemIndex val (graphVertices g))

traverseNeighbors_ :: Applicative m
  => (e -> Vertex g -> v -> m a)
  -> Vertex g
  -> Graph g e v
  -> m ()
traverseNeighbors_ f (Vertex x) g =
  let allVertices = graphVertices g
      vertices = graphOutboundNeighborVertices g V.! x
      edges    = graphOutboundNeighborEdges g V.! x
      numNeighbors = U.length vertices
      go !i = if i < numNeighbors
        then let vertexNum = vertices U.! i
                 vertexVal = allVertices V.! vertexNum
                 edgeVal = edges V.! i
              in f edgeVal (Vertex vertexNum) vertexVal *> go (i + 1)
        else pure ()
   in go 0

-- lookupEdge :: Vertex g -> Vertex g -> Graph g e v -> Maybe (Edge g)
-- lookupEdge (Vertex x) (Vertex y) g =

mutableIForM_ :: PrimMonad m => MVector (PrimState m) a -> (Int -> a -> m b) -> m ()
mutableIForM_ m f = forM_ (take (MV.length m) (enumFrom 0)) $ \i -> do
  a <- MV.read m i
  f i a

mutableIFoldM' :: PrimMonad m => (a -> Int -> b -> m a) -> a -> MVector (PrimState m) b -> m a
mutableIFoldM' f x m = go 0 x where
  len = MV.length m
  go !i !a = if i < len
    then do
      b <- MV.read m i
      aNext <- f a i b
      go (i + 1) aNext
    else return x

