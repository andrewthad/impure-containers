{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

-- | This module provides a safe and performant way to perform
--   operations on graphs. The types 'Graph', 'Vertex', 'Vertices'
--   and 'Size' are all parameterized by a phantom type variable @g@.
--   Much like the @s@ used with @ST@, this type variable will
--   always be free. It gives us a guarentee that a vertex belongs
--   in a graph. See the bottom of this page for a more detailed
--   explanation.

module Data.Graph.Immutable
  ( -- * Graph Operations
    lookupVertex
  , lookupEdge
  , atVertex
  , mapVertices
  , mapEdges
  , traverseVertices_
  , traverseEdges_
  , traverseNeighbors_
  , vertices
  , setVertices
  , size
  , freeze
  , create
  , with
  , mapSome
    -- * Algorithms
  , dijkstra
  , dijkstraDistance
  , dijkstraFoldM
    -- * Size and Vertex
  , sizeInt
  , vertexInt
    -- * Vertices
  , verticesRead
  , verticesLength
  , verticesTraverse
  , verticesTraverse_
  , verticesToVertexList
  , verticesToVector
  , verticesThaw
  , verticesFreeze
  ) where

import Data.Graph.Types.Internal
import Control.Monad.Primitive
import Data.Foldable
import Data.Vector (Vector)
import Data.Vector.Mutable (MVector)
import Data.Functor.Identity (Identity(..))
import Control.Monad
import Data.Word
import Control.Monad.ST (runST)
import Data.Primitive.MutVar
import Data.Coerce (coerce)
import qualified Data.Graph.Mutable as Mutable
import qualified Data.ArrayList.Generic as ArrayList
import qualified Data.HashMap.Mutable.Basic as HashTable
import qualified Data.Heap.Mutable.ModelD as Heap
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

-- | Lookup a 'Vertex' by its label.
lookupVertex :: Eq v => v -> Graph g e v -> Maybe (Vertex g)
lookupVertex val (Graph g) = fmap Vertex (V.elemIndex val (graphVertices g))

lookupEdge :: Vertex g -> Vertex g -> Graph g e v -> Maybe e
lookupEdge (Vertex x) (Vertex y) (Graph (SomeGraph _ neighbors edges)) =
  case U.elemIndex y (V.unsafeIndex neighbors x) of
    Nothing -> Nothing
    Just ix -> Just (V.unsafeIndex (V.unsafeIndex edges x) ix)

atVertex :: Vertex g -> Graph g e v -> v
atVertex v g = verticesRead (vertices g) v

-- | Not the same as 'fmap' because the function also takes the vertex id.
mapVertices :: (Vertex g -> a -> b) -> Graph g e a -> Graph g e b
mapVertices f (Graph sg) = Graph sg
  { graphVertices = V.imap (coerce f) (graphVertices sg) }

-- | Map of the edges in the graph.
mapEdges :: (Vertex g -> Vertex g -> e -> d) -> Graph g e v -> Graph g d v
mapEdges f (Graph (SomeGraph v verts edges)) = Graph $ SomeGraph v verts $ 
  V.imap
    ( \outerIx edgeVals ->
        let vertIxs = V.unsafeIndex verts outerIx
         in V.imap
              ( \sourceIx edgeVal -> 
                  let destIx = U.unsafeIndex vertIxs sourceIx
                   in f (Vertex sourceIx) (Vertex destIx) edgeVal
              ) edgeVals
    ) edges

traverseVertices_ :: Applicative m => (Vertex g -> v -> m a) -> Graph g e v -> m ()
traverseVertices_ f g = verticesTraverse_ f (vertices g)

-- | This traverses every edge in the entire graph.
traverseEdges_ :: Applicative m
  => (Vertex g -> Vertex g -> v -> v -> e -> m a)
  -> Graph g e v
  -> m ()
traverseEdges_ f g =
  let allVertices = vertices g
   in verticesTraverse_
        (\vertex value -> traverseNeighbors_
          (\neighborVertex neighborValue e -> f vertex neighborVertex value neighborValue e)
          vertex g
        ) allVertices

-- | Traverse the neighbors of a specific vertex.
traverseNeighbors_ :: Applicative m
  => (Vertex g -> v -> e -> m a)
  -> Vertex g
  -> Graph g e v
  -> m ()
traverseNeighbors_ f (Vertex x) (Graph g) =
  let allVertices  = graphVertices g
      theVertices  = V.unsafeIndex (graphOutboundNeighborVertices g) x
      edges        = V.unsafeIndex (graphOutboundNeighborEdges g) x
      numNeighbors = U.length theVertices
      go !i = if i < numNeighbors
        then let vertexNum = U.unsafeIndex theVertices i
                 vertexVal = V.unsafeIndex allVertices vertexNum
                 edgeVal = V.unsafeIndex edges i
              in f (Vertex vertexNum) vertexVal edgeVal *> go (i + 1)
        else pure ()
   in go 0


-- | Get the vertices from a graph.
vertices :: Graph g e v -> Vertices g v
vertices (Graph (SomeGraph v _ _)) = Vertices v

-- | Set the vertices of a graph.
setVertices :: Vertices g v -> Graph g e w -> Graph g e v
setVertices (Vertices x) (Graph (SomeGraph _ a b)) = Graph (SomeGraph x a b)

-- | Get the number of vertices in a graph.
size :: Graph g e v -> Size g
size (Graph (SomeGraph v _ _)) = Size (V.length v)

sizeInt :: Size g -> Int
sizeInt (Size s) = s

-- | Convert a 'Vertex' to an 'Int'.
vertexInt :: Vertex g -> Int
vertexInt (Vertex i) = i

verticesToVertexList :: Vertices g v -> [Vertex g]
verticesToVertexList (Vertices v) = map Vertex (take (V.length v) [0..])

-- | This is currently inefficient. If an @itraverse@ gets added
--   to @vector@, this can be made faster.
verticesTraverse :: Applicative m => (Vertex g -> v -> m a) -> Vertices g v -> m (Vertices g a)
verticesTraverse f (Vertices v) = fmap (Vertices . V.fromList) $ traverse (\(i,b) -> f (Vertex i) b) (zip [0..] (V.toList v))
  -- Vertices (V.imapM (\i -> f (Vertex i)) v)

-- | This is currently inefficient. If an @itraverse@ gets added
--   to @vector@, this can be made faster.
verticesTraverse_ :: Applicative m => (Vertex g -> v -> m a) -> Vertices g v -> m ()
verticesTraverse_ f (Vertices v) = traverse_ (\(i,b) -> f (Vertex i) b) (zip [0..] (V.toList v))
  -- V.imapM_ (\i -> f (Vertex i)) v

verticesToVector :: Vertices g v -> Vector v
verticesToVector (Vertices v) = v

verticesRead :: Vertices g v -> Vertex g -> v
verticesRead (Vertices v) (Vertex i) = V.unsafeIndex v i

verticesLength :: Vertices g v -> Int
verticesLength (Vertices v) = V.length v

verticesFreeze :: PrimMonad m => MVertices (PrimState m) g v -> m (Vertices g v)
verticesFreeze (MVertices mvec) = fmap Vertices (V.freeze mvec)

-- | Make a mutable copy of a set of 'Vertices'.
verticesThaw :: PrimMonad m => Vertices g v -> m (MVertices (PrimState m) g v)
verticesThaw (Vertices vec) = fmap MVertices (V.thaw vec)

-- | Make an immutable copy of a mutable graph.
freeze :: PrimMonad m => MGraph (PrimState m) g e v -> m (Graph g e v)
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
create :: PrimMonad m => (forall g. MGraph (PrimState m) g e v -> m ()) -> m (SomeGraph e v)
create f = do
  mg <- MGraph
    <$> HashTable.new
    <*> newMutVar 0
    <*> HashTable.new
  f mg
  Graph g <- freeze mg
  return g

-- | Take a function that can be performed on any 'Graph' and perform that
--   on the given 'SomeGraph'.
with :: SomeGraph e v -> (forall g. Graph g e v -> a) -> a
with sg f = f (Graph sg)

-- | Lift a 'Graph' morphism into a 'SomeGraph' morphism.
mapSome :: (forall g. Graph g e v -> Graph g e' v') -> SomeGraph e v -> SomeGraph e' v'
mapSome f g = case f (Graph g) of
  Graph g' -> g'

-- | Find the shortest path between two vertices using Dijkstra\'s algorithm.
--   The source code of this function provides an example of how to use
--   the generalized variants of Dijkstra\'s algorithm provided by this
--   module.
dijkstraDistance :: (Num e, Ord e)
  => Vertex g -- ^ Start vertex
  -> Vertex g -- ^ End vertex
  -> Graph g e v -- ^ Graph
  -> Maybe e
dijkstraDistance start end g = 
  getMinDistance $ atVertex end
  ( dijkstra
    (\_ _ mdist e -> addMinDistance mdist e)
    (MinDistance (Just 0))
    (Identity start) g
  )
  where addMinDistance (MinDistance m) e = MinDistance (fmap (+ e) m)

newtype MinDistance a = MinDistance { getMinDistance :: Maybe a }

instance Eq a => Eq (MinDistance a) where
  MinDistance a == MinDistance b = a == b

instance Ord a => Ord (MinDistance a) where
  compare (MinDistance a) (MinDistance b) = case a of
    Nothing -> case b of
      Nothing -> EQ
      Just _ -> GT
    Just aval -> case b of
      Nothing -> LT
      Just bval -> compare aval bval

instance Ord a => Monoid (MinDistance a) where
  mempty = MinDistance Nothing
  mappend ma mb = min ma mb

-- | This is a generalization of Dijkstra\'s algorithm. Like the original,
--   it takes a start 'Vertex' but unlike the original, it does not take
--   an end. It will continue traversing the 'Graph' until it has touched
--   all vertices that are reachable from the start vertex.
--
--   Additionally, this function generalizes the notion of distance. It
--   can be numeric (as Dijkstra has it) data, non-numeric data, or tagged numeric
--   data. This can be used, for example, to find the shortest path from
--   the start vertex to all other vertices in the graph.
--
--   In Dijkstra\'s original algorithm, tentative distances are initialized
--   to infinity. After a node is visited, the procedure for updating its
--   neighbors\' tentative distance to a node is to compare the existing tentative distance with
--   the new one and to keep the lesser of the two.
--
--   In this variant, tentative distances are initialized to 'mempty'.
--   The update procedure involves combining them with 'mappend' instead
--   of being choosing the smaller of the two. For this
--   algorithm to function correctly, the distance @s@ must have 'Ord' and
--   'Monoid' instances satisfying:
--
--   > ∀ a b. mappend a b ≤ a
--   > ∀ a b. mappend a b ≤ b
--   > ∀ c.   mempty ≥ c
--
--   Additionally, the 'Monoid' instance should have a commutative 'mappend':
--
--   > ∀ a b. mappend a b ≅ mappend b a
--
--   The weight function is described by:
--
--   > from    to    from   edge   tentative
--   > node   node  weight  value  to weight
--   >  |      |      |      |      |
--   >  V      V      V      V      V
--   >
--   > (v  ->  v  ->  s  ->  e  ->  s)
--
--   In many cases, some of input values can be ignored. For example, to implement
--   Dijkstra\'s original algorithm the @from-node@ and @to-node@ values are
--   not needed. The weight combining function will typically use the @from-weight@
--   in some way. The way this algorithm uses the weight function makes it suseptible to
--   the same negative-edge problem as the original. For some weight combining
--   function @f@, it should be the case that:
--
--   > ∀ v1 v2 s e. f v1 v2 s e ≥ s
--
--   This function could be written without unsafely pattern matching
--   on 'Vertex', but doing so allows us to use a faster heap implementation.
dijkstra ::
     (Ord s, Monoid s, Foldable t)
  => (v -> v -> s -> e -> s) -- ^ Weight function
  -> s -- ^ Weight to assign start vertex
  -> t (Vertex g) -- ^ Start vertices
  -> Graph g e v -- ^ Graph
  -> Graph g e s
dijkstra f s0 v0 g = 
  fst $ runST $ dijkstraGeneral f (\_ _ _ -> return ()) s0 () v0 g

-- Traverse every vertex in the graph and monadically fold
-- their values.
dijkstraFoldM :: 
     (Ord s, Monoid s, Foldable t, PrimMonad m)
  => (v -> v -> s -> e -> s) -- ^ Weight function
  -> (v -> s -> x -> m x) -- ^ Monadic fold function
  -> s -- ^ Weight to assign start vertex
  -> x -- ^ Initial accumulator
  -> t (Vertex g) -- ^ Start vertices
  -> Graph g e v -- ^ Graph
  -> m x
dijkstraFoldM f mf s0 acc v0 g = 
  fmap snd $ dijkstraGeneral f mf s0 acc v0 g

-- | This is not exported
dijkstraGeneral ::
     (Ord s, Monoid s, Foldable t, PrimMonad m)
  => (v -> v -> s -> e -> s) -- ^ Weight function
  -> (v -> s -> x -> m x) -- ^ Monadic fold
  -> s -- ^ Weight to assign start vertex
  -> x -- ^ Initial fold value
  -> t (Vertex g) -- ^ Start vertices
  -> Graph g e v -- ^ Graph
  -> m (Graph g e s, x)
dijkstraGeneral f step s0 x0 v0 g = do
  let theSize = size g
      oldVertices = vertices g
  newVertices <- Mutable.verticesReplicate theSize mempty
  visited <- Mutable.verticesUReplicate theSize False
  heap <- Heap.new (sizeInt theSize)
  forM_ v0 $ \v -> do
    Mutable.verticesWrite newVertices v s0
    -- Using getVertex casts Vertex to Int. This is safe to do,
    -- but going from Int to Vertex (done later) is normally unsafe.
    -- We know it's ok in this case because the min heap does not
    -- create Ints that we did not push onto it.
    Heap.unsafePush s0 (getVertexInternal v) heap
  let go x = do
        m <- Heap.pop heap
        case m of
          Nothing -> return (BoolWith True x)
          Just (s,unwrappedVertexIx) -> do
            -- Unsafe cast from Int to Vertex
            let vertex = Vertex unwrappedVertexIx
                value = verticesRead oldVertices vertex
            Mutable.verticesUWrite visited vertex True
            Mutable.verticesWrite newVertices vertex s
            traverseNeighbors_ (\neighborVertex neighborValue theEdge -> do
                alreadyVisited <- Mutable.verticesURead visited neighborVertex
                when (not alreadyVisited) $ Heap.unsafePush
                  (f value neighborValue s theEdge)
                  -- Casting from Vertex to Int
                  (getVertexInternal neighborVertex)
                  heap
              ) vertex g
            xNext <- step value s x
            return (BoolWith False xNext)
      runMe x = do
        BoolWith isDone xNext <- go x
        if isDone then return xNext else runMe xNext
  xFinal <- runMe x0
  newVerticesFrozen <- verticesFreeze newVertices
  return (setVertices newVerticesFrozen g, xFinal)

data BoolWith a = BoolWith Bool !a

-- mutableIForM_ :: PrimMonad m => MVector (PrimState m) a -> (Int -> a -> m b) -> m ()
-- mutableIForM_ m f = forM_ (take (MV.length m) (enumFrom 0)) $ \i -> do
--   a <- MV.read m i
--   f i a
--
-- mutableIFoldM' :: PrimMonad m => (a -> Int -> b -> m a) -> a -> MVector (PrimState m) b -> m a
-- mutableIFoldM' f x m = go 0 x where
--   len = MV.length m
--   go !i !a = if i < len
--     then do
--       b <- MV.read m i
--       aNext <- f a i b
--       go (i + 1) aNext
--     else return x
