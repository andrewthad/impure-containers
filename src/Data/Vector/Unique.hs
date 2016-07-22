{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Unique where

import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Vector.Mutable as M
import Data.Vector.Mutable (MVector)
import Control.Monad.ST (ST)

{-@ measure mvlen :: MVector s a -> Int @-}
{-@ invariant {mv:MVector s a | 0 <= mvlen mv } @-}

{-@ assume mnew :: n:Nat -> ST s ({mv:MVector s a | mvlen mv == n}) @-}
mnew :: Int -> ST s (M.MVector s a)
mnew = M.new

{-@ assume mtake :: n:Nat -> in:{in:MVector s a | n <= mvlen mv } -> {out:MVector s a | mvlen out = n } @-}
mtake :: Int -> MVector s a -> MVector s a
mtake = M.take

{-@ assume mdrop :: n:Nat ->
                    in:{in:MVector s a | n <= mvlen in } ->
                    {out:MVector s a | mvlen out = mvlen in - n } @-}
mdrop :: Int -> MVector s a -> MVector s a
mdrop = M.drop

{-@ assume vcopy :: target:MVector s a ->
                    source:{source:Vector a | mvlen target = vlen source } ->
                    ST s () @-}
vcopy :: MVector s a -> Vector a -> ST s ()
vcopy = V.copy

{-@ assume V.drop :: n:Nat ->
                     in:{in:Vector a | n <= vlen in } ->
                     {out:Vector a | vlen out = vlen in - n } @-}

{-@ unionV :: v1:Vector -> v2:Vector a -> Vector a @-}
unionV :: forall a. Ord a => V.Vector a -> V.Vector a -> V.Vector a
unionV v1 v2 = V.create (mnew (V.length v1 + V.length v2) >>= go 0 0 0)
    {-@ go :: sindex1:{n:Nat | n <= vlen v1 } ->
              sindex2:{n:Nat | n <= vlen v2 } ->
              tindex:{n:Nat | true } ->
              target:{mv:MVector s a | tindex <= mvlen mv } ->
              ST s (MVector s a) @-}
  where go :: Int -> Int -> Int -> MVector s a -> ST s (MVector s a)
        go sindex1 sindex2 tindex target = case V.length v1 <= sindex1 of
          True -> do
            vcopy (mtake (V.length v2 - sindex2) (mdrop tindex target)) (V.drop sindex2 v2)
            pure target -- (M.take (tindex + V.length v2 - sindex2) target)
          False -> pure target

