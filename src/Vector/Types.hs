{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | All of the folding functions in this module are strict in the accumulator.
module Vector.Types
  ( -- * Integer
    Index
  , Length
    -- * Immutable
  , BoxedVector
  , UnboxedVector
    -- * Mutable
  , MutableBoxedVector
  , MutableUnboxedVector
    -- * Families
  , Mutable
  , Element
  ) where

import Vector.Unsafe

