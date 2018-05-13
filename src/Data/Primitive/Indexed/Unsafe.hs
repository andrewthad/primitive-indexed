{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Primitive.Indexed.Unsafe
  ( Index(..)
  , Length(..)
    -- * Immutable
  , Vector(..)
  , PrimVector(..)
    -- * Mutable
  , MutableVector(..)
  , MutablePrimVector(..)
  ) where

import Data.Primitive

-- | An integer that can be used to index into an array of length @n@.
newtype Index n = Index { getIndex :: Int }
  deriving (Eq,Ord,Show,Prim)
type role Index nominal

-- | A value-level representation of length @n@.
newtype Length n = Length { getLength :: Int }
  deriving (Show,Prim)
type role Length nominal

instance Eq (Length n) where
  _ == _ = True
  _ /= _ = False

instance Ord (Length n) where
  compare _ _ = EQ

newtype Vector n a = Vector (Array a)
  deriving (Eq,Ord,Functor,Foldable,Traversable)
type role Vector nominal representational

newtype MutableVector n s a = MutableVector (MutableArray s a)
type role MutableVector nominal nominal representational

newtype PrimVector n a = PrimVector (PrimArray a)
  deriving (Eq,Ord)
type role PrimVector nominal nominal

newtype MutablePrimVector n s a = MutablePrimVector (MutablePrimArray s a)
type role MutablePrimVector nominal nominal representational

