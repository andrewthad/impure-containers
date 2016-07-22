{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Unique where

import Prelude hiding (null, head, tail, notElem, empty)
import Data.Set (Set, singleton, union, empty)
import Data.Monoid ((<>))

import Data.Vector hiding (singleton, empty)
import qualified Data.Vector.Mutable as M
import Control.Monad.Primitive
import qualified Data.Vector as V
import Control.Monad.ST (ST)

uncons :: Vector a -> Maybe (a,Vector a)
uncons vec = case null vec of
  True -> Nothing
  False -> Just (head vec, tail vec)

{-@ Lazy unique @-}
{-@ measure unique :: Vector a -> Prop @-}
unique :: (Ord a) => Vector a -> Bool
unique vec = case uncons vec of
  Nothing -> True
  Just (v,vs) -> v `notElem` vs && unique vs

{-@ type UVector a = {vs:Vector a | unique vs }@-}

{-@ Lazy elemsV @-}
{-@ measure elemsV :: Vector a -> Set a @-}
elemsV :: Ord a => Vector a -> Set a
elemsV vec = case uncons vec of
  Nothing -> empty
  Just (v,vs) -> singleton v `union` elemsV vs

{-@ measure elemsL :: [a] -> Set a @-}
elemsL :: Ord a => [a] -> Set a
elemsL [] = empty
elemsL (x:xs) = singleton x `union` elemsL xs

{-@ Lazy distinctInc @-}
{-@ measure distinctInc :: Vector a -> Prop @-}
distinctInc :: Ord a => Vector a -> Bool
distinctInc vec = case uncons vec of
  Nothing -> True
  Just (v,vs) -> V.all (v <) vs && distinctInc vs

{-@ type DistinctIncVector a = {vs:Vector a | distinctInc vs } @-}
{-@ assume V.singleton :: v:a -> {vec:Vector a | Set_sng v = elemsV vec && distinctInc vec } @-}
{-@ assume V.cons :: v:a ->
                     vs:{vs:Vector a | not (Set_mem v (elemsV vs)) && distinctInc vs } ->
                     {vec:Vector a | Set_cup (Set_sng v) (elemsV vs) = elemsV vec && distinctInc vec } @-}

{-@ test :: DistinctIncVector Int @-}
test :: Vector Int
test = V.cons 10 (V.cons 20 (V.singleton 30))

-- {- test1 :: DistinctIncVector Int @-}
-- test1 :: Vector Int
-- test1 = V.cons 10 $ V.cons 20 $ V.singleton 30

 --

{-@ assume M.copy :: PrimMonad m => v1:MVector (PrimState m) a ->
                                    v2:{v2:MVector (PrimState m) a | mvlen v1 == mvlen v2 } ->
                                    m () @-}
{-@ assume V.copy :: PrimMonad m => v1:MVector (PrimState m) a ->
                                    v2:{v2:Vector a | mvlen v1 = vlen v2} ->
                                    m () @-}
{- assume M.drop :: n:Nat -> in:MVector s a ->
                     {out:MVector s a | if (n <= mvlen in) then (mvlen out == mvlen in - n) else (mvlen out == 0) } @-}
{-@ assume M.drop :: n:Nat -> in:{in:MVector s a | true } ->
                     {out:MVector s a | mvlen out == mvlen in - n } @-}
{- assume V.drop :: n:Nat -> in:Vector a ->
                     {out:Vector a | if (n <= vlen in) then (vlen out == vlen in - n) else (vlen out == 0) } @-}
{-@ assume V.drop :: n:Nat -> in:{in:Vector a | n <= vlen in } ->
                     {out:Vector a | vlen out == vlen in - n } @-}
{- assume M.take :: n:Nat -> in:MVector s a ->
                     {out:MVector s a | if (n <= mvlen in) then (mvlen out == n) else (mvlen out == mvlen in) } @-}
{-@ assume M.take :: n:Nat -> in:{in:MVector s a | n <= mvlen in } ->
                     {out:MVector s a | mvlen out == n } @-}
{-@ assume V.thaw :: (PrimMonad m) => in:Vector a -> m ({out:MVector (PrimState m) a | mvlen out == vlen in }) @-}

{- unionV :: v1:DistinctIncVector a ->
              v2:DistinctIncVector a ->
              {vec:Vector a | Set_cup (elemsV v1) (elemsV v2) = (elemsV vec) && distinctInc vec } @-}
{-@ unionV :: v1:DistinctIncVector a ->
              v2:DistinctIncVector a ->
              {vec:Vector a | true } @-}
unionV :: forall a. Ord a => Vector a -> Vector a -> Vector a
unionV v1 v2 = create (M.new (V.length v1 + V.length v2) >>= go 0 0 0)
    {-@ go :: PrimMonad m => sindex1:{n:Nat | n <= vlen v1 } ->
                             sindex2:{n:Nat | n <= vlen v2 } ->
                             tindex:Nat ->
                             target:{in:MVector (PrimState m) a | tindex <= mvlen in } ->
                             m ({out:MVector (PrimState m) a | true}) @-}
  where go :: PrimMonad m => Int ->
                             Int ->
                             Int ->
                             MVector (PrimState m) a ->
                             m (MVector (PrimState m) a)
        go sindex1 sindex2 tindex target = case V.length v1 <= sindex1 of
          True -> do
            -- V.copy (M.take (V.length v2 - sindex2) (M.drop tindex target)) (V.drop sindex2 v2)
            pure target --(M.take (tindex + V.length v2 - sindex2) target)
          False -> pure target
          -- case V.length v2 <= sindex2 of
          --   True -> do
          --     source1 <- thaw v1
          --     M.copy (M.take (V.length v1 - sindex1) (M.drop tindex target)) (M.drop sindex1 source1)
          --     pure (M.take (tindex + V.length v1 - sindex1) target)
          --   False -> do
          --     pure target

        -- go :: PrimMonad m => MVector (PrimState m) a -> Int -> Int -> Int -> m (MVector (PrimState m) a)
        -- go target sindex1 sindex2 tindex
        --   | sindex1 >= V.length v1 = do
        --       source2 <- thaw v2
        --       M.copy (M.take (V.length v2 - sindex2) (M.drop tindex target)) (M.drop sindex2 source2)
        --       pure (M.take (tindex + V.length v2 - sindex2) target)
        --   | sindex2 >= V.length v2 = do
        --       source1 <- thaw v1
        --       M.copy (M.take (V.length v1 - sindex1) (M.drop tindex target)) (M.drop sindex1 source1)
        --       pure (M.take (tindex + V.length v1 - sindex1) target)
        --   | v1 ! sindex1 < v2 ! sindex2 = do
        --       M.write target tindex (v1 ! sindex1)
        --       go target (sindex1 + 1) sindex2 (tindex + 1)
        --   | v1 ! sindex1 == v2 ! sindex2 = do
        --       M.write target tindex (v1 ! sindex1)
        --       go target (sindex1 + 1) (sindex2 + 1) (tindex + 1)
        --   | otherwise = do
        --       M.write target tindex (v2 ! sindex1)
        --       go target sindex1 (sindex2 + 1) (tindex + 1)

{-@ assume new' :: n:Nat -> ST s ({v:MVector s a | mvlen v == n}) @-}
new' :: Int -> ST s (MVector s a)
new' = M.new

{-@ measure mvlen :: forall a. (MVector s a) -> Int @-}
{-@ invariant {mv:MVector s a | 0 <= mvlen mv } @-}
{-@ assume M.new :: (PrimMonad m) => n:Nat -> m ({vs:MVector (PrimState m) a | mvlen vs == n }) @-}
{-@ minimal :: v:Vector a -> Vector a @-}
minimal :: Vector a -> Vector a
minimal v = create (M.new (V.length v) >>= go)
    {-@ go :: in:{in:MVector s a | mvlen in == vlen v } -> ST s (MVector s a) @-}
  where go :: MVector s a -> ST s (MVector s a)
        go mv = pure mv
