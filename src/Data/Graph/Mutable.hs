module Data.Graph.Mutable where

import Data.Graph.Types
import Control.Monad.Primitive
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Vector.Unboxed (Unbox)
import Data.Primitive.MutVar
import Data.Hashable (Hashable)
import qualified Data.HashMap.Mutable.Basic as HashTable

verticesReplicate :: PrimMonad m => Size g -> v -> m (MVertices g (PrimState m) v)
verticesReplicate (Size i) v = fmap MVertices (MV.replicate i v)

verticesUReplicate :: (PrimMonad m, Unbox v) => Size g -> v -> m (MUVertices g (PrimState m) v)
verticesUReplicate (Size i) v = fmap MUVertices (MU.replicate i v)

verticesUWrite :: (PrimMonad m, Unbox v) => MUVertices g (PrimState m) v -> Vertex g -> v -> m ()
verticesUWrite (MUVertices mvec) (Vertex ix) v = MU.unsafeWrite mvec ix v

verticesWrite :: PrimMonad m => MVertices g (PrimState m) v -> Vertex g -> v -> m ()
verticesWrite (MVertices mvec) (Vertex ix) v = MV.unsafeWrite mvec ix v

verticesURead :: (PrimMonad m, Unbox v) => MUVertices g (PrimState m) v -> Vertex g -> m v
verticesURead (MUVertices mvec) (Vertex ix) = MU.unsafeRead mvec ix

verticesRead :: PrimMonad m => MVertices g (PrimState m) v -> Vertex g -> m v
verticesRead (MVertices mvec) (Vertex ix) = MV.unsafeRead mvec ix

-- | This does two things:
--
--   * Check to see if a vertex with the provided value already exists
--   * Create a new vertex if it does not exist
--
--   In either case, the vertex id is returned, regardless or whether it was
--   preexisting or newly created.
insertVertex :: (PrimMonad m, Hashable v, Eq v) => MGraph g (PrimState m) e v -> v -> m (Vertex g)
insertVertex (MGraph vertexIndex currentIdVar _) v = do
  m <- HashTable.lookup vertexIndex v
  case m of
    Nothing -> do
      currentId <- readMutVar currentIdVar
      writeMutVar currentIdVar (currentId + 1)
      HashTable.insert vertexIndex v currentId
      return (Vertex currentId)
    Just i -> return (Vertex i)

-- | This replaces the edge if it already exists. If you pass the same vertex
--   as the source and the destination, this function has no effect.
insertEdge :: PrimMonad m => MGraph g (PrimState m) e v -> Vertex g -> Vertex g -> e -> m ()
insertEdge (MGraph _ _ edges) (Vertex a) (Vertex b) e = do
  HashTable.insert edges (IntPair a b) e

-- | Insert edge with a function, combining the existing edge value and the old one.
insertEdgeWith :: PrimMonad m => MGraph g (PrimState m) e v -> (e -> e -> e) -> Vertex g -> Vertex g -> e -> m ()
insertEdgeWith (MGraph _ _ edges) combine (Vertex a) (Vertex b) e = do
  m <- HashTable.lookup edges (IntPair a b)
  case m of
    Nothing -> HashTable.insert edges (IntPair a b) e
    Just eOld -> HashTable.insert edges (IntPair a b) (combine eOld e)


