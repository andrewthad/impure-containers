module Lib
    ( someFunc
    ) where

import           Control.Monad.Primitive
import           Data.Vector.Mutable
                 (IOVector, MVector, unsafeNew, unsafeWrite)
import qualified Data.Vector.Mutable     as MV

{-@ measure mvlen    :: Data.Vector.MVector s a -> Int @-}
{-@ invariant       { v : Data.Vector.MVector s a | 0 <= mvlen v } @-}

{-@ assume MV.replicate :: PrimMonad m => x : Nat -> a -> ( m { v : ( MVector ( PrimState m ) a ) | mvlen v = x } ) @-}

{-@ assume unsafeWrite :: Control.Monad.Primitive.PrimMonad m
      => x:(Data.Vector.MVector (Control.Monad.Primitive.PrimState m) a)
      -> ix:{v:Nat | v < mvlen x }
      -> a
      -> m ()
@-}

{-@ testWrite :: PrimMonad m => x:(MVector (PrimState m) a) -> ix:{v:Nat | v < mvlen x } -> a -> (a -> a) -> m () @-}
testWrite :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> (a -> a) -> m ()
testWrite mv ix a f = unsafeWrite mv ix (f a)

{-@ otherWrite :: PrimMonad m => x:(MVector (PrimState m) a) -> ix:{v:Nat | v < mvlen x } -> a -> a -> m () @-}
otherWrite :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> a -> m ()
otherWrite mv ix a b = unsafeWrite mv ix b

{-@ add6 :: i:{i:Int | i < 94} -> {j:Int| j = i + 6} @-}
add6 :: Int -> Int
add6 x = x + 6

{-@
data Tuple a b <p :: a -> b -> Prop> = Tuple (x::a) (b<p x>)
@-}
data Tuple a b = Tuple a b

data List a = Nil | Cons a (List a)
{-@
data List a <p :: a -> a -> Prop>
  = Nil
  | Cons (x :: a) (xs :: List <p> (a<p x>))
@-}

{-@ test :: (List Int)<{\x y -> x < y}> @-}
test :: List Int
test = Cons 5 ( Cons 4 (Cons 6 Nil))

{-@ testFunc :: PrimMonad m => m ( MVector (PrimState m) ( Int<{\x -> x < 100}> ) ) @-}
testFunc :: PrimMonad m => m (MVector (PrimState m) Int)
testFunc = do
  v <- MV.replicate 12 99
  unsafeWrite v 2 55
  unsafeWrite v 3 99
  -- This line fails as expected:
  -- testWrite v 4 10 add6
  -- This line succeeds as expected:
  -- otherWrite v 4 99 98
  -- This line fails, and I think it's understandable why:
  -- otherWrite v 4 99 98
  return v

  -- myVec <- restrictedNew 8
  -- restrictedWrite myVec 6 55
  -- return myVec
  -- restrictedReplicate 12 6

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-@ assume unsafeNew :: PrimMonad m => x : Nat -> ( m { v : ( MVector ( PrimState m ) a ) | mvlen v = x } ) @-}

{-@ restrictedWrite :: forall <p :: a -> Prop>. PrimMonad m => mv:MVector (PrimState m) a<p> -> {ix:Nat | ix < mvlen mv} -> a<p> -> m () @-}
restrictedWrite :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()
restrictedWrite mv ix a = unsafeWrite mv ix a

{-@ restrictedNew :: forall <p :: a -> Prop>. PrimMonad m => x : Nat -> ( m { v : ( MVector ( PrimState m ) a<p> ) | mvlen v = x } ) @-}
restrictedNew :: PrimMonad m => Int -> m (MVector (PrimState m) a)
restrictedNew = unsafeNew

{-@ restrictedReplicate :: forall <p :: a -> Prop>. PrimMonad m => x : Nat -> a<p> -> ( m { v : ( MVector ( PrimState m ) (a<p>)) | mvlen v = x } ) @-}
restrictedReplicate :: PrimMonad m => Int -> a -> m (MVector (PrimState m) a)
restrictedReplicate = MV.replicate

