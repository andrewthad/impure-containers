{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns  #-}
module Data.Graph.Types where

import Data.HashMap.Mutable.Basic (HashTable)
import Data.Vector (Vector,MVector)
import Data.Primitive.MutVar (MutVar)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

newtype Graph g e v = Graph { getGraphInternal :: SomeGraph e v }
  deriving (Functor)

-- | The neighbor vertices and neighbor edges must have
--   equal length.
--
--   TODO: enforce that the inner vectors for the neighbors are
--   ordered. This will make testing for neighbors easier and
--   will make an equality check easier.
data SomeGraph e v = SomeGraph
  { graphVertices :: !(Vector v)
  , graphOutboundNeighborVertices :: !(Vector (U.Vector Int))
  , graphOutboundNeighborEdges :: !(Vector (Vector e))
  } deriving (Functor)

newtype Size g = Size { getSizeInternal :: Int }

newtype Vertex g = Vertex { getVertexInternal :: Int }
  deriving (Eq,Ord,Hashable)
newtype Vertices g v = Vertices { getVerticesInternal :: Vector v }
  deriving (Functor)
newtype MVertices g s v = MVertices { getMVerticesInternal :: MVector s v }
newtype MUVertices g s v = MUVertices { getMUVerticesInternal :: MU.MVector s v }

data IntPair = IntPair !Int !Int
  deriving (Eq,Ord,Show,Read,Generic)

instance Hashable IntPair

data MGraph g s e v = MGraph
  { mgraphVertexIndex :: !(HashTable s v Int)
  , mgraphCurrentId :: !(MutVar s Int)
  , mgraphEdges :: !(HashTable s IntPair e)
  }

