{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language UnboxedTuples #-}

module Control.Monad.ST.Run
  ( -- * Words
    runIntST
  , runWordST
    -- * Primitive Arrays
  , runArrayST
  , runSmallArrayST
  , runByteArrayST
  , runPrimArrayST
  , runUnliftedArrayST
  ) where

import GHC.Int (Int(I#))
import GHC.Word (Word(W#))
import GHC.ST (ST(..))
import GHC.Exts (runRW#)
import Data.Primitive (Array(..),SmallArray(..),ByteArray(..),PrimArray(..),UnliftedArray(..))

{-# INLINE runIntST #-}
runIntST :: (forall s. ST s Int) -> Int
runIntST (ST x) =
  case runRW# (\s0 -> case x s0 of (# s1, I# y #) -> (# s1, y #)) of
    (# _, y #) -> I# y

{-# INLINE runWordST #-}
runWordST :: (forall s. ST s Word) -> Word
runWordST (ST x) =
  case runRW# (\s0 -> case x s0 of (# s1, W# y #) -> (# s1, y #)) of
    (# _, y #) -> W# y

{-# INLINE runArrayST #-}
runArrayST :: (forall s. ST s (Array a)) -> Array a
runArrayST (ST x) =
  case runRW# (\s0 -> case x s0 of (# s1, Array y #) -> (# s1, y #)) of
    (# _, y #) -> Array y

{-# INLINE runSmallArrayST #-}
runSmallArrayST :: (forall s. ST s (SmallArray a)) -> SmallArray a
runSmallArrayST (ST x) =
  case runRW# (\s0 -> case x s0 of (# s1, SmallArray y #) -> (# s1, y #)) of
    (# _, y #) -> SmallArray y

{-# INLINE runByteArrayST #-}
runByteArrayST :: (forall s. ST s ByteArray) -> ByteArray
runByteArrayST (ST x) =
  case runRW# (\s0 -> case x s0 of (# s1, ByteArray y #) -> (# s1, y #)) of
    (# _, y #) -> ByteArray y

{-# INLINE runPrimArrayST #-}
runPrimArrayST :: (forall s. ST s (PrimArray a)) -> PrimArray a
runPrimArrayST (ST x) =
  case runRW# (\s0 -> case x s0 of (# s1, PrimArray y #) -> (# s1, y #)) of
    (# _, y #) -> PrimArray y

{-# INLINE runUnliftedArrayST #-}
runUnliftedArrayST :: (forall s. ST s (UnliftedArray a)) -> UnliftedArray a
runUnliftedArrayST (ST x) =
  case runRW# (\s0 -> case x s0 of (# s1, UnliftedArray y #) -> (# s1, y #)) of
    (# _, y #) -> UnliftedArray y

