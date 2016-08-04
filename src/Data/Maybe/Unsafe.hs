{-# LANGUAGE MagicHash, BangPatterns #-}
module Data.Maybe.Unsafe (UnsafeMaybe
                         ,just
                         ,nothing
                         ,fromMaybe
                         ,maybe
                         ,toMaybe) where

import Unsafe.Coerce
import GHC.Prim
import Prelude hiding (maybe)

thunk :: Int -> Int
thunk x = error "Data.Maybe.Unsafe.nothingSurrogate evaluated"
{-# NOINLINE thunk #-}

-- | nothingSurrogate stands in for the value Nothing; we distinguish it by pointer
nothingSurrogate :: Any
nothingSurrogate = unsafeCoerce thunk
{-# NOINLINE nothingSurrogate #-}

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
fromMaybe (Just a) = unsafeCoerce a
fromMaybe Nothing  = UnsafeMaybe nothingSurrogate
{-# INLINE fromMaybe #-}

maybe :: b -> (a -> b) -> UnsafeMaybe a -> b
maybe !def transform (UnsafeMaybe a) = case reallyUnsafePtrEquality# (a `seq` a) nothingSurrogate of
  0# -> transform (unsafeCoerce a)
  _  -> def
{-# INLINE maybe #-}

-- toMaybe :: UnsafeMaybe a -> Maybe a
-- toMaybe (UnsafeMaybe a) = case reallyUnsafePtrEquality# a nothingSurrogate of
--   0# -> Just (unsafeCoerce a)
--   _  -> Nothing
-- {-# INLINE toMaybe #-}

toMaybe :: UnsafeMaybe a -> Maybe a
toMaybe = maybe Nothing Just
