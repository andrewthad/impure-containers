{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

module Data.Graph.Immutable.Tagged where

import Data.Graph.Types
import Control.Monad.Primitive
import Data.Vector (Vector)
import Data.Vector.Mutable (MVector)
import Control.Monad
import Data.Word
import Control.Monad.ST (runST)
import Data.Primitive.MutVar
import qualified Data.Graph.Mutable.Tagged as Mutable
import qualified Data.ArrayList.Generic as ArrayList
import qualified Data.HashMap.Mutable.Basic as HashTable
import qualified Data.Heap.Mutable.ModelD as Heap
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

mapVertices :: (v -> w) -> Graph g e v -> Graph g e w
mapVertices = fmap

-- | This is a generalization of Dijkstra\'s algorithm. This function could
--   be written without unsafely pattern matching on 'Vectex', but doing
--   so allows us to use a faster heap implementation.
dijkstraTraversal ::
     (Ord s, Monoid s)
  => (v -> v -> s -> e -> s)
  -> Vertex g
  -> Graph g e v
  -> Vertices g s
dijkstraTraversal f v0 g = runST $ do
  let theSize = size g
      oldVertices = vertices g
  newVertices <- Mutable.replicateVertex theSize mempty
  visited <- Mutable.replicateUVertex theSize False
  heap <- Heap.new (unSize theSize)
  -- Using getVertex casts Vertex to Int. This is safe to do,
  -- but going from Int to Vertex (done later) is normally unsafe.
  -- We know it's ok in this case because the min heap does not
  -- create Ints that we did not push onto it.
  Heap.unsafePush mempty (getVertexInternal v0) heap
  let go = do
        m <- Heap.pop heap
        case m of
          Nothing -> return True
          Just (s,unwrappedVertexIx) -> do
            -- Unsafe cast from Int to Vertex
            let vertex = Vertex unwrappedVertexIx
                value = verticesRead oldVertices vertex
            Mutable.writeUVertex visited vertex True
            Mutable.writeVertex newVertices vertex s
            traverseNeighbors_ (\theEdge neighborVertex neighborValue -> do
                alreadyVisited <- Mutable.readUVertex visited neighborVertex
                when (not alreadyVisited) $ Heap.unsafePush
                  (f value neighborValue s theEdge)
                  -- Casting from Vertex to Int
                  (getVertexInternal neighborVertex)
                  heap
              ) vertex g
            return False
      runMe = do
        isDone <- go
        if isDone then return () else runMe
  runMe
  newVerticesFrozen <- verticesFreeze newVertices
  return newVerticesFrozen

lookupVertex :: Eq v => v -> Graph g e v -> Maybe (Vertex g)
lookupVertex val (Graph g) = fmap Vertex (V.elemIndex val (graphVertices g))

traverseNeighbors_ :: Applicative m
  => (e -> Vertex g -> v -> m a)
  -> Vertex g
  -> Graph g e v
  -> m ()
traverseNeighbors_ f (Vertex x) (Graph g) =
  let allVertices = graphVertices g
      theVertices = graphOutboundNeighborVertices g V.! x
      edges    = graphOutboundNeighborEdges g V.! x
      numNeighbors = U.length theVertices
      go !i = if i < numNeighbors
        then let vertexNum = theVertices U.! i
                 vertexVal = allVertices V.! vertexNum
                 edgeVal = edges V.! i
              in f edgeVal (Vertex vertexNum) vertexVal *> go (i + 1)
        else pure ()
   in go 0

lookupEdge :: Vertex g -> Vertex g -> Graph g e v -> Maybe e
lookupEdge (Vertex x) (Vertex y) (Graph (SomeGraph _ neighbors edges)) =
  case U.elemIndex y (V.unsafeIndex neighbors x) of
    Nothing -> Nothing
    Just ix -> Just (V.unsafeIndex (V.unsafeIndex edges x) ix)

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

vertices :: Graph g e v -> Vertices g v
vertices (Graph (SomeGraph v _ _)) = Vertices v

size :: Graph g e v -> Size g
size (Graph (SomeGraph v _ _)) = Size (V.length v)

unSize :: Size g -> Int
unSize (Size s) = s

vertexInt :: Vertex g -> Int
vertexInt (Vertex i) = i

verticesRead :: Vertices g v -> Vertex g -> v
verticesRead (Vertices v) (Vertex i) = V.unsafeIndex v i

verticesLength :: Vertices g v -> Int
verticesLength (Vertices v) = V.length v

verticesFreeze :: PrimMonad m => MVertices g (PrimState m) v -> m (Vertices g v)
verticesFreeze (MVertices mvec) = fmap Vertices (V.freeze mvec)

verticesThaw :: PrimMonad m => Vertices g v -> m (MVertices g (PrimState m) v)
verticesThaw (Vertices vec) = fmap MVertices (V.thaw vec)

freeze :: PrimMonad m => MGraph g (PrimState m) e v -> m (Graph g e v)
freeze (MGraph vertexIndex currentIdVar edges) = do
  let initialArrayListSize = 16
  numberOfVertices <- readMutVar currentIdVar
  mvec <- MV.new numberOfVertices
  mvecEdgeVals <- MV.replicateM numberOfVertices (ArrayList.new initialArrayListSize)
  mvecEdgeNeighbors <- MV.replicateM numberOfVertices (ArrayList.new initialArrayListSize)
  flip HashTable.mapM_ vertexIndex $ \vertexValue vertexId -> do
    MV.unsafeWrite mvec vertexId vertexValue
  flip HashTable.mapM_ edges $ \(IntPair fromVertexId toVertexId) edgeVal -> do
    -- This would be better if I used hybrid vectors.
    mvecEdgeVal <- MV.unsafeRead mvecEdgeVals fromVertexId
    ArrayList.push mvecEdgeVal edgeVal
    mvecEdgeNeighbor <- MV.unsafeRead mvecEdgeNeighbors fromVertexId
    ArrayList.push mvecEdgeNeighbor toVertexId
  vecEdgeVals1 <- V.unsafeFreeze mvecEdgeVals
  vecEdgeVals2 <- V.mapM ArrayList.freeze vecEdgeVals1
  vecEdgeNeighbors1 <- V.unsafeFreeze mvecEdgeNeighbors
  vecEdgeNeighbors2 <- V.mapM ArrayList.freeze vecEdgeNeighbors1
  vec <- V.unsafeFreeze mvec
  return (Graph $ SomeGraph vec vecEdgeNeighbors2 vecEdgeVals2)

-- | Takes a function that builds on an empty 'MGraph'. After the function
--   mutates the 'MGraph', it is frozen and becomes an immutable 'SomeGraph'.
create :: PrimMonad m => (forall g. MGraph g (PrimState m) e v -> m ()) -> m (SomeGraph e v)
create f = do
  mg <- MGraph
    <$> HashTable.new
    <*> newMutVar 0
    <*> HashTable.new
  f mg
  Graph g <- freeze mg
  return g

with :: (forall g. Graph g e v -> a) -> SomeGraph e v -> a
with f sg = f (Graph sg)

-- data Edge g = Edge
--   { edgeVertexA :: !Int
--   , edgeVertexB :: !Int
--   }

-- instance Functor (Graph g e) where
--   fmap f g = g { graphVertices = Vector.map (graphVertices g) }

-- visited,allowed,notAllowed :: Word8
-- visited = 2
-- allowed = 1
-- notAllowed = 0
