{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns  #-}
module Data.Graph.Types
  ( SomeGraph
  , Graph
  , Vertex
  , Size
  , Vertices
  , MVertices
  , MUVertices
  , MGraph
  , IOGraph
  , STGraph
  ) where

import Data.Graph.Types.Internal

