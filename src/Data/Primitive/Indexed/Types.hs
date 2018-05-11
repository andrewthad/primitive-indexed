{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

-- | All of the folding functions in this module are strict in the accumulator.
module Data.Primitive.Indexed.Types
  ( Index
  , Length
  , Vector
  , ascendM
  , descendM
  , ascend
  , descend
  , reflect
  ) where

import Data.Primitive.Indexed.Unsafe

-- | A strict left monadic fold over the ascending indices from zero up to
-- a given length.
ascendM :: forall m n a. (Monoid a, Monad m) => (Index n -> m a) -> Length n -> m a
{-# INLINE ascendM #-}
ascendM f (Length n) = go 0 mempty
  where
  go :: Int -> a -> m a
  go !ix !a = if ix < n
    then f (Index ix) >>= go (ix + 1)
    else pure a

-- | A strict monadic left fold over the descending indices from a given length
-- down to zero.
descendM :: forall m n a. (Monoid a, Monad m) => (Index n -> m a) -> Length n -> m a
{-# INLINE descendM #-}
descendM f (Length n) = go (n - 1) mempty
  where
  go :: Int -> a -> m a
  go !ix !a = if ix >= 0
    then f (Index ix) >>= go (ix - 1)
    else pure a

-- | A strict left fold over the ascending indices from zero up to
-- a given length.
ascend :: forall n a. (a -> Index n -> a) -> a -> Length n -> a
{-# INLINE ascend #-}
ascend f a0 (Length n) = go 0 a0
  where
  go :: Int -> a -> a
  go !ix !a = if ix < n
    then go (ix + 1) (f a (Index ix))
    else a

-- | A strict left fold over the descending indices from a given length
-- down to zero.
descend :: forall n a. (a -> Index n -> a) -> a -> Length n -> a
{-# INLINE descend #-}
descend f a0 (Length n) = go (n - 1) a0
  where
  go :: Int -> a -> a
  go !ix !a = if ix >= 0
    then go (ix - 1) (f a (Index ix))
    else a

-- | Reflect an index about the middle of the length. For a length of 5 this
-- has the following effect:
--
-- > 0 ==> 4
-- > 1 ==> 3
-- > 2 ==> 2
-- > 3 ==> 1
-- > 4 ==> 0
reflect :: Length n -> Index n -> Index n
{-# INLINE reflect #-}
reflect (Length n) (Index i) = Index ((n - i) - 1)

