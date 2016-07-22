{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Unique where

import Prelude hiding (null, head, tail, notElem, empty)
import qualified Data.Set as S

import Data.Vector hiding (singleton, empty, all)
import qualified Data.Vector.Mutable as M
import Control.Monad.Primitive
import qualified Data.Vector as V

{-@ measure elems @-}
elems :: Ord a => [a] -> S.Set a
elems [] = S.empty
elems (x:xs) = S.singleton x `S.union` elems xs

{-@ measure distinctInc @-}
distinctInc :: Ord a => [a] -> Bool
distinctInc [] = True
distinctInc (x:xs) = lessThan x xs && distinctInc xs

{-@ measure lessThan @-}
lessThan :: Ord a => a -> [a] -> Bool
lessThan x [] = True
lessThan x (y:ys)
  | x < y = lessThan x ys
  | otherwise = False


{-@ assume myCons :: x:a ->
                  xs:{xs:[a] | distinctInc xs } ->
                  {out:[a] | distinctInc out }@-}
myCons = (:)

{-@ Lazy unionL @-}
{-@ unionL :: Ord a => l1:{l1:[a] | distinctInc l1 } ->
                       l2:{l2:[a] | distinctInc l2 } ->
                       {out:[a] | elems out = Set_cup (elems l1) (elems l2) } @-}
unionL :: Ord a => [a] -> [a] -> [a]
unionL [] l2 = l2
unionL l1 [] = l1
unionL (x:xs) (y:ys)
  | x < y  = x `myCons` unionL xs (y:ys)
  | x == y = x `myCons` unionL xs ys
  | x > y  = y `myCons` unionL (x:xs) ys
