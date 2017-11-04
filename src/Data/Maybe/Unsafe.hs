{-# LANGUAGE MagicHash, BangPatterns #-}
module Data.Maybe.Unsafe (UnsafeMaybe
                         ,just
                         ,nothing
                         ,fromMaybe
                         ,maybe
                         ,toMaybe) where

import Unsafe.Coerce
import System.IO.Unsafe
import System.Mem.StableName
import GHC.Prim
import GHC.Types
import Prelude hiding (maybe)

thunk :: Int -> Int
thunk x = error "bang"
{-# NOINLINE thunk #-}

thunkStableName :: StableName (Int -> Int)
thunkStableName = unsafePerformIO (makeStableName thunk)

-- | nothingSurrogate stands in for the value Nothing; we distinguish it by pointer
nothingSurrogate :: Any
nothingSurrogate = unsafeCoerce thunk
{-# NOINLINE nothingSurrogate #-}

nothingStableName :: StableName Any
nothingStableName = unsafePerformIO (makeStableName nothingSurrogate)

newtype UnsafeMaybe a = UnsafeMaybe Any

instance Functor UnsafeMaybe where
  fmap f = maybe nothing (just . f)

instance Applicative UnsafeMaybe where
  pure = just
  {-# INLINE pure #-}
  -- (UnsafeMaybe f) <*> (UnsafeMaybe x) = case reallyUnsafePtrEquality# f nothingSurrogate of
  --   0# -> case reallyUnsafePtrEquality# x nothingSurrogate of
  --     0# -> just ((unsafeCoerce f) (unsafeCoerce x))
  --     _  -> nothing
  --   _  -> nothing
  mf <*> mx = maybe nothing (\f -> maybe nothing (just . f) mx) mf
  {-# INLINE (<*>) #-}

instance Monad UnsafeMaybe where
  return = just
  -- (UnsafeMaybe x) >>= f = case reallyUnsafePtrEquality# x nothingSurrogate of
  --   0# -> f (unsafeCoerce x)
  --   _  -> nothing
  mx >>= f = maybe nothing f mx


just :: a -> UnsafeMaybe a
just a = UnsafeMaybe (unsafeCoerce a)

nothing :: UnsafeMaybe a
nothing = UnsafeMaybe nothingSurrogate

fromMaybe :: Maybe a -> UnsafeMaybe a
fromMaybe (Just a) = just a
fromMaybe Nothing  = nothing
{-# INLINE fromMaybe #-}

maybe :: b -> (a -> b) -> UnsafeMaybe a -> b
maybe !def transform (UnsafeMaybe !a) = case eqStableName thunkStableName named ||
                                            eqStableName nothingStableName named of
  False -> transform (unsafeCoerce a)
  True  -> def
  where named = unsafePerformIO (makeStableName a)
{-# INLINE maybe #-}

-- toMaybe :: UnsafeMaybe a -> Maybe a
-- toMaybe (UnsafeMaybe a) = case reallyUnsafePtrEquality# a nothingSurrogate of
--   0# -> Just (unsafeCoerce a)
--   _  -> Nothing
-- {-# INLINE toMaybe #-}

toMaybe :: UnsafeMaybe a -> Maybe a
toMaybe = maybe Nothing Just
