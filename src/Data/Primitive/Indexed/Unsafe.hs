{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | This module exports data constructors that subvert the guarantees
-- provided by this library. It is part of the stable API, but users
-- should importing this take on the burden of proving that these
-- are used safely. If there is something this module is required for,
-- please open an issue on github.
module Data.Primitive.Indexed.Unsafe
  ( -- * Integer
    Index(..)
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

