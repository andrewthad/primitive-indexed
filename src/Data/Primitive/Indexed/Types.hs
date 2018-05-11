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
  , bound
  ) where

import Data.Primitive.Indexed.Unsafe

ascendM :: forall m n a. (Monoid a, Monad m) => (Index n -> m a) -> Length n -> m a
{-# INLINE ascendM #-}
ascendM f (Length n) = go 0 mempty
  where
  go :: Int -> a -> m a
  go !ix !a = if ix < n
    then f (Index ix) >>= go (ix + 1)
    else pure a

descendM :: forall m n a. (Monoid a, Monad m) => (Index n -> m a) -> Length n -> m a
{-# INLINE descendM #-}
descendM f (Length n) = go (n - 1) mempty
  where
  go :: Int -> a -> m a
  go !ix !a = if ix >= 0
    then f (Index ix) >>= go (ix - 1)
    else pure a

-- | A strict left fold over the the ascending indices from zero up to
-- a given length.
ascend :: forall n a. (a -> Index n -> a) -> a -> Length n -> a
{-# INLINE ascend #-}
ascend f a0 (Length n) = go 0 a0
  where
  go :: Int -> a -> a
  go !ix !a = if ix < n
    then go (ix + 1) (f a (Index ix))
    else a

-- | A strict left fold over the the descending indices from a given length
-- down to zero.
descend :: forall n a. (a -> Index n -> a) -> a -> Length n -> a
{-# INLINE descend #-}
descend f a0 (Length n) = go (n - 1) a0
  where
  go :: Int -> a -> a
  go !ix !a = if ix >= 0
    then go (ix - 1) (f a (Index ix))
    else a

-- | Given an 'Index', return the smallest 'Length' that contains the index.
bound :: Index n -> Length n
{-# INLINE bound #-}
bound (Index n) = Length (n + 1)

