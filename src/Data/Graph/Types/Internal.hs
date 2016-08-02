{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns  #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Unsafe Internals
--
-- The internals provided here do not constitute part of the stable API.
-- Additionally, they are unsafe. Using these data constructors directly
-- can cause other functions in this library to segfault. If you find that
-- you need something from this module, consider opening up an issue on
-- github so that the functionality you need can be provided by the safe
-- API instead.
--
module Data.Graph.Types.Internal where

import Data.HashMap.Mutable.Basic (MHashMap)
import Data.Vector (Vector,MVector)
import Data.Primitive.MutVar (MutVar)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.Monad.ST (RealWorld)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

-- | A 'Graph' with edges labeled by @e@ and vertices labeled by @v@.
--   The @g@ type variable is a phatom type that associates a
--   'Graph' with vertices that belong to it.
newtype Graph g e v = Graph { getGraphInternal :: SomeGraph e v }
  deriving (Functor)

-- | This is a 'Graph' without the phantom type variable. Very few
--   functions work with this type.
data SomeGraph e v = SomeGraph
  { graphVertices                 :: !(Vector v)
  , graphOutboundNeighborVertices :: !(Vector (U.Vector Int))
  , graphOutboundNeighborEdges    :: !(Vector (Vector e))
  } deriving (Functor, Eq, Ord)
-- The neighbor vertices and neighbor edges must have
-- equal length.
--
-- TODO: enforce that the inner vectors for the neighbors are
-- ordered. This will make testing for neighbors easier and
-- will make an equality check easier.

newtype Size g = Size { getSizeInternal :: Int }

-- | A reference to a vertex in a 'Graph' with matching type variable @g@.
--   'Vertex' is a thin wrapper for 'Int' and does not hold the label
--   of the vertex.
newtype Vertex g = Vertex { getVertexInternal :: Int }
  deriving (Eq,Ord,Hashable)

-- | All vertices in a 'Graph' with matching type variable @g@.
newtype Vertices g v = Vertices { getVerticesInternal :: Vector v }
  deriving (Functor)

-- | Mutable vertices that have the same length as the vertices in a 'Graph'.
--   This is used to safely implement algorithms that need to mark vertices
--   as they traverse a graph.
newtype MVertices s g v = MVertices { getMVerticesInternal :: MVector s v }

-- | Mutable unboxed vertices that have the same length as the vertices in a 'Graph'.
--   See 'MVertices'.
newtype MUVertices s g v = MUVertices { getMUVerticesInternal :: MU.MVector s v }

-- | A strict pair of 'Int's. This is used internally.
data IntPair = IntPair !Int !Int
  deriving (Eq,Ord,Show,Read,Generic)

instance Hashable IntPair

-- | This is more accurately thought of as a graph builder rather than a mutable
--   graph. You can add vertices and edges, and you can delete edges, but you cannot
--   delete vertices.
data MGraph s g e v = MGraph
  { mgraphVertexIndex :: !(MHashMap s v Int)
  , mgraphCurrentId :: !(MutVar s Int)
  , mgraphEdges :: !(MHashMap s IntPair e)
  }

type IOGraph = MGraph RealWorld
type STGraph s = MGraph s

