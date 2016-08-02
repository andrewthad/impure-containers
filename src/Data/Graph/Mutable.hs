module Data.Graph.Mutable
  ( -- * Graph Operations
    -- $mutgraph
    insertVertex
  , insertEdge
  , insertEdgeWith
    -- * Vertices Operations
    -- $mutvertices
  , verticesReplicate
  , verticesUReplicate
  , verticesWrite
  , verticesUWrite
  , verticesRead
  , verticesURead
  ) where

import Data.Graph.Types.Internal
import Control.Monad.Primitive
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Vector.Unboxed (Unbox)
import Data.Primitive.MutVar
import Data.Hashable (Hashable)
import qualified Data.HashMap.Mutable.Basic as HashTable

-- | $mutgraph
-- Operations that mutate a 'MGraph'. Vertices and edges can both be added,
-- and edges can be deleted, but vertices cannot be deleted. Providing such
-- an operation would undermine the safety that this library provides.

-- | This does two things:
--
--   * Check to see if a vertex with the provided value already exists
--   * Create a new vertex if it does not exist
--
--   In either case, the vertex id is returned, regardless or whether it was
--   preexisting or newly created.
insertVertex :: (PrimMonad m, Hashable v, Eq v) => MGraph (PrimState m) g e v -> v -> m (Vertex g)
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
insertEdge :: PrimMonad m => MGraph (PrimState m) g e v -> Vertex g -> Vertex g -> e -> m ()
insertEdge (MGraph _ _ edges) (Vertex a) (Vertex b) e = do
  HashTable.insert edges (IntPair a b) e

-- | Insert edge with a function, combining the existing edge value and the old one.
insertEdgeWith :: PrimMonad m => MGraph (PrimState m) g e v -> (e -> e -> e) -> Vertex g -> Vertex g -> e -> m ()
insertEdgeWith (MGraph _ _ edges) combine (Vertex a) (Vertex b) e = do
  m <- HashTable.lookup edges (IntPair a b)
  case m of
    Nothing -> HashTable.insert edges (IntPair a b) e
    Just eOld -> HashTable.insert edges (IntPair a b) (combine eOld e)

-- | $mutvertices
-- Operations that mutate a 'MVertices' or a 'MUVertices'. These functions have nothing
-- to do with 'MGraph' and are not usually needed by end users of this library. They
-- are useful for users writing algorithms that need to mark vertices in a graph as
-- it is traversed.
--
-- All of these operations are
-- wrappers around operations from @Data.Vector.Mutable@ and @Data.Vector.Unbox.Mutable@.
-- As long as you do not import @Data.Graph.Types.Internal@, this library guarentees that
-- these operations will not fail at runtime.

verticesReplicate :: PrimMonad m => Size g -> v -> m (MVertices (PrimState m) g v)
verticesReplicate (Size i) v = fmap MVertices (MV.replicate i v)

verticesUReplicate :: (PrimMonad m, Unbox v) => Size g -> v -> m (MUVertices (PrimState m) g v)
verticesUReplicate (Size i) v = fmap MUVertices (MU.replicate i v)

verticesUWrite :: (PrimMonad m, Unbox v) => MUVertices (PrimState m) g v -> Vertex g -> v -> m ()
verticesUWrite (MUVertices mvec) (Vertex ix) v = MU.unsafeWrite mvec ix v

verticesWrite :: PrimMonad m => MVertices (PrimState m) g v -> Vertex g -> v -> m ()
verticesWrite (MVertices mvec) (Vertex ix) v = MV.unsafeWrite mvec ix v

verticesURead :: (PrimMonad m, Unbox v) => MUVertices (PrimState m) g v -> Vertex g -> m v
verticesURead (MUVertices mvec) (Vertex ix) = MU.unsafeRead mvec ix

verticesRead :: PrimMonad m => MVertices (PrimState m) g v -> Vertex g -> m v
verticesRead (MVertices mvec) (Vertex ix) = MV.unsafeRead mvec ix

