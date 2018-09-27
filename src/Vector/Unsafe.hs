{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# language TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | This module exports data constructors that subvert the guarantees
-- provided by this library. It is part of the stable API, but users
-- should importing this take on the burden of proving that these
-- are used safely. If there is something this module is required for,
-- please open an issue on github.
module Vector.Unsafe
  ( -- * Integer
    Index(..)
  , Length(..)
    -- * Immutable
  , BoxedVector(..)
  , UnboxedVector(..)
    -- * Mutable
  , MutableBoxedVector(..)
  , MutableUnboxedVector(..)
    -- * Families
  , Mutable
  , Element
  ) where

import Data.Kind (Type)
import Data.Primitive
import GHC.TypeLits (Nat)

-- | An integer that can be used to index into an array of length @n@.
newtype Index (n :: Nat) = Index { getIndex :: Int }
  deriving (Eq,Ord,Show,Prim)
type role Index nominal

-- | A value-level representation of length @n@.
newtype Length (n :: Nat) = Length { getLength :: Int }
  deriving (Show,Prim)
type role Length nominal

instance Eq (Length n) where
  _ == _ = True
  _ /= _ = False

instance Ord (Length n) where
  compare _ _ = EQ

newtype BoxedVector (n :: Nat) a = BoxedVector (Array a)
  deriving (Eq,Ord,Functor,Foldable,Traversable)
type role BoxedVector nominal representational

newtype MutableBoxedVector s (n :: Nat) a = MutableBoxedVector (MutableArray s a)
type role MutableBoxedVector nominal nominal representational

newtype UnboxedVector (n :: Nat) a = UnboxedVector (PrimArray a)
  deriving (Eq,Ord)
type role UnboxedVector nominal nominal

newtype MutableUnboxedVector s (n :: Nat) a = MutableUnboxedVector (MutablePrimArray s a)
type role MutableUnboxedVector nominal nominal representational

type instance Mutable BoxedVector = MutableBoxedVector
type instance Mutable UnboxedVector = MutableUnboxedVector

type family Element (v :: Nat -> Type) :: Type
type family Mutable (v :: Nat -> k) = (m :: Type -> Nat -> k) | m -> v

