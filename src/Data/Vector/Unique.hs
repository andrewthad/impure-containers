{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Vector.Unique where

import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Vector.Mutable as M
import Data.Vector.Mutable (MVector)
import Control.Monad.ST (ST)
import Test.Target.Targetable
import GHC.Generics (Generic)

-- deriving instance Generic (MVector s a)
-- instance Targetable (MVector s a)

{-@ measure mvlen :: MVector s a -> Int @-}
{-@ invariant {mv:MVector s a | 0 <= mvlen mv } @-}

{-@ assume mnew :: n:Nat -> ST s ({mv:MVector s a | mvlen mv == n}) @-}
mnew :: Int -> ST s (M.MVector s a)
mnew = M.new

{-@ assume mtake :: n:Nat -> in:{in:MVector s a | n <= mvlen in } -> {out:MVector s a | mvlen out = n } @-}
mtake :: Int -> MVector s a -> MVector s a
mtake = M.take

{-@ assume mdrop :: n:Nat ->
                    in:{in:MVector s a | n <= mvlen in } ->
                    {out:MVector s a | mvlen out = mvlen in - n } @-}
mdrop :: Int -> MVector s a -> MVector s a
mdrop = M.drop

{-@ assume mslice :: start:Nat -> length:Nat ->
                     {in:MVector s a | start + length <= mvlen in } ->
                     {out:MVector s a | mvlen out = length } @-}
mslice :: Int -> Int -> MVector s a -> MVector s a
mslice = M.slice

{-@ assume vcopy :: target:MVector s a ->
                    source:{source:Vector a | mvlen target = vlen source } ->
                    ST s () @-}
vcopy :: MVector s a -> Vector a -> ST s ()
vcopy = V.copy

{-@ assume V.drop :: n:Nat ->
                     in:{in:Vector a | n <= vlen in } ->
                     {out:Vector a | vlen out = vlen in - n } @-}

{-  unionV :: v1:Vector -> v2:Vector a -> Vector a @-}
unionV :: forall a. Ord a => V.Vector a -> V.Vector a -> V.Vector a
unionV v1X v2X = V.create (mnew (V.length v1X + V.length v2X) >>= go v1X v2X 0 0 0)
    {-@ go :: v1:Vector a ->
              v2:Vector a ->
              sindex1:{n:Nat | n <= vlen v1 } ->
              sindex2:{n:Nat | n <= vlen v2 } ->
              tindex:Nat ->
              target:{mv:MVector s a | tindex + (vlen v2 - sindex2) <= mvlen mv } ->
              ST s (MVector s a) @-}
  where go :: Int -> Int -> Int -> MVector s a -> ST s (MVector s a)
        go sindex1 sindex2 tindex target = case V.length v1 == sindex1 of
          True -> do
            vcopy (mslice tindex (V.length v2 - sindex2) target) (V.drop sindex2 v2)
            pure (mtake (tindex + V.length v2 - sindex2) target)
          False -> pure target

