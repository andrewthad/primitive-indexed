{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

-- | All of the folding functions in this module are strict in the accumulator.
module Data.Primitive.Indexed.Types
  ( Index
  , Length
  , Vector
  , PrimVector
  , ascendM
  , descendM
  , ascend
  , descend
  , reflect
  , offset
  , zero
  , unindex
  , unlength
  ) where

import Data.Primitive.Indexed.Unsafe
import GHC.Exts (remInt#,Int(I#))

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

-- | Convert an index to an integer, discarding the information
-- about how it relates to a length.
unindex :: Index n -> Int
unindex (Index n) = n

-- | Convert a length to an integer, discarding the information
-- about how it relates to indices and other lengths.
unlength :: Length n -> Int
unlength (Length n) = n

-- | The existence of any index is evidence that there an index
-- into the zero position is valid.
zero :: Index n -> Index n
zero _ = Index 0

-- | Add an offset to an index and reduce in modulo the length
-- to ensure that the resulting index is in bounds.
offset :: Length n -> Int -> Index n -> Index n
offset (Length len) off (Index ix) = Index (unsafeRem (ix + off) len)

unsafeRem :: Int -> Int -> Int
unsafeRem (I# a) (I# b) = I# (remInt# a b)
