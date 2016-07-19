{-# LANGUAGE CPP #-}

module Data.HashMap.Mutable.Internal.Array
  ( MutableArray
  , newArray
  , readArray
  , writeArray
  ) where


import           Control.Monad.ST
import           Control.Monad.Primitive (PrimMonad,PrimState)

#ifdef BOUNDS_CHECKING
import qualified Data.Vector.Mutable as M
import           Data.Vector.Mutable (MVector)
#else
import qualified Data.Primitive.Array as M
import           Data.Primitive.Array (MutableArray)
#endif


#ifdef BOUNDS_CHECKING

type MutableArray s a = MVector s a

newArray :: PrimMonad m => Int -> a -> m (MutableArray (PrimState m) a)
newArray = M.replicate

readArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> m a
readArray = M.read

writeArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> a -> m ()
writeArray = M.write

#else

newArray :: PrimMonad m => Int -> a -> m (MutableArray (PrimState m) a)
newArray = M.newArray

readArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> m a
readArray = M.readArray

writeArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> a -> m ()
writeArray = M.writeArray

#endif
