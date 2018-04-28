{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTSyntax   #-}
{-# LANGUAGE RankNTypes   #-}

module Data.Graph.Immutable.Basic where

import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as MV
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU

data Vertex g = Vertex { getVertex :: Int }

data SomeGraph e v where
  SomeGraph :: { getSomeGraph :: Graph g e v } -> SomeGraph e v

data Graph g e v = Graph
  { graphVertices          :: Vector v
  , graphOutboundNeighbors :: Vector (U.Vector Int)
  , graphEdges             :: Int -> Int -> e
  }

visited,allowed,notAllowed :: Word8
visited = 2
allowed = 1
notAllowed = 0

-- breadthFirstBy :: (s -> s -> Ordering) -> (s -> s -> e -> v -> v -> s) -> (v -> s) -> Vertex g -> Graph g e v -> Graph g e s
-- breadthFirstBy cmp f f0 (Vertex v0) (Graph vertices _ _) = runST $ do
--   let vertexCount = V.length vertices
--   -- The values in newVertices are tentative until
--   -- visited becomes true.
--   newVertices <- MV.new vertexCount
--   MV.set newVertices s0
--   statusOfVertices <- MU.new vertexCount
--   MV.set statusOfVertices notAllowed
--   MV.write statusOfVertices i v0 allowed
--   let keepGoing = do
--         mutableIFoldM' (\i a -> do
--           vertexCount
--           )
--   keepGoing


lookupVertex :: Eq v => v -> Graph g e v -> Maybe (Vertex g)
lookupVertex val g = fmap Vertex (Vector.elem (graphVertices g))


mutableIForM_ :: PrimMonad m => MVector (PrimState m) a -> (Int -> a -> m b) -> m ()
mutableIForM_ m f = forM_ (take (MV.length m) (enumFrom 0)) $ \i -> do
  a <- MV.read m i
  f i a

mutableIFoldM' :: PrimMonad m => (a -> Int -> b -> m a) -> a -> MVector (PrimState m) b -> m a
mutableIFoldM' f x m = go 0 where
  len = MVector.length m
  go !i !a = do
    if i < len
      then do
        b <- MVector.read m i a
        aNext <- f a i b
        go (i + 1) aNext
      else return x

