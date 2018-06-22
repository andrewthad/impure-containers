--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module ImpureContainers.Misc.Mobility
  ( Mobility (Mobile, Pinned)
  , M
  , P
  ) where

--------------------------------------------------------------------------------

-- | FIXME: doc
data Mobility
  = -- | FIXME: doc
    Mobile
  | -- | FIXME: doc
    Pinned
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

--------------------------------------------------------------------------------

-- | FIXME: doc
type M = 'Mobile

-- | FIXME: doc
type P = 'Pinned

--------------------------------------------------------------------------------
