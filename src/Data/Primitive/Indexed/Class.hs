{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTSyntax #-}
{-# language GADTs #-}
{-# language MagicHash #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Data.Primitive.Indexed.Class
  ( Element
  , Mutable
  , Container(..)
  , PolyContainer(..)
  , ContainerProduct(..)
  , MutableContainerProduct(..)
  , Elem(..)
  ) where

import Prelude hiding (replicate)

import Control.Applicative (liftA2)
import Control.Monad.ST (ST)
import Data.Primitive.Indexed.Types (Length,Index)
import GHC.TypeLits (Nat)
import Data.Kind (Type)
import Control.Monad.Primitive (PrimState,PrimMonad)
import GHC.Generics
import Data.Proxy
import GHC.Exts (RealWorld,Proxy#,proxy#)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Primitive.Indexed.Array as A
import qualified Data.Primitive.Indexed.Unsafe as U

data Elem = Poly | Mono

type family Element (v :: Nat -> Type) :: Type
type family Mutable (v :: Nat -> k) = (m :: Type -> Nat -> k) | m -> v

class Container (v :: Nat -> Type) where
  replicate :: Length n -> Element v -> ST s (Mutable v s n)
  index :: v n -> Index n -> Element v

class PolyContainer (v :: Nat -> Type -> Type) where
  indexPoly :: v n a -> Index n -> a
  replicatePoly :: Length n -> a -> ST s (Mutable v s n a)

newtype ContainerProduct (es :: [Elem]) v n = ContainerProduct (v n)
newtype MutableContainerProduct (es :: [Elem]) (v :: Nat -> Type) s n = MutableContainerProduct (Mutable v s n)

type instance Element (ContainerProduct es v) = Element v
type instance Mutable (ContainerProduct es v) = MutableContainerProduct es v

unsafeCoerceST :: ST RealWorld a -> ST s a
unsafeCoerceST = unsafeCoerce

unsafeCoerceMutableST :: ST RealWorld (MutableContainerProduct es v RealWorld 42) -> ST s (MutableContainerProduct es v s n)
unsafeCoerceMutableST = unsafeCoerce

class GContainerProduct (es :: [Elem]) (v :: Type -> Type) (m :: Type -> Type) (e :: Type -> Type) where
  -- gproductNew :: PrimMonad m => Proxy e -> Length n -> m (m (PrimState m) n)
  gcontainerProductReplicate :: Proxy# es -> Proxy# v -> Length n -> e x -> ST RealWorld (m x)
  gcontainerProductIndex :: Proxy# es -> Proxy# m -> v x -> Index n -> e x
  -- gcontainerProductWrite :: Proxy# v -> m n -> Index n -> e x -> ST RealWorld ()

instance forall (es :: [Elem]) (v :: Nat -> Type) (m :: Type -> Nat -> Type).
       (GContainerProduct es (Rep (v 42)) (Rep (m RealWorld 42)) (Rep (Element v)), Generic (v 42), Generic (m RealWorld 42), Generic (Element v), m ~ Mutable v)
    => Container (ContainerProduct es v) where
  {-# INLINE index #-}
  index = indexHelper
  {-# INLINE replicate #-}
  replicate !len a = unsafeCoerceMutableST $ do
    r <- gcontainerProductReplicate 
      (proxy# :: Proxy# es)
      (proxy# :: Proxy# (Rep (v 42)))
      (U.Length (U.getLength len))
      (from a)
    return (MutableContainerProduct (to r))

{-# INLINE indexHelper #-}
indexHelper :: forall (es :: [Elem]) v n. (GContainerProduct es (Rep (v 42)) (Rep (Mutable v RealWorld 42)) (Rep (Element v)), Generic (Element v), Generic (v 42)) => ContainerProduct es v n -> Index n -> Element v
indexHelper (ContainerProduct !v) !ix = to
  ( gcontainerProductIndex
      (proxy# :: Proxy# es)
      (proxy# :: Proxy# (Rep (Mutable v RealWorld 42)))
      (from ((unsafeCoerce :: v n -> v 42) v))
      (U.Index (U.getIndex ix))
  )

instance forall (es :: [Elem]) (v :: Type -> Type) (m :: Type -> Type) (e :: Type -> Type) i c1 c2 c3. (GContainerProduct es v m e) => GContainerProduct es (M1 i c1 v) (M1 i c2 m) (M1 i c3 e) where
  {-# INLINE gcontainerProductIndex #-}
  gcontainerProductIndex _ _ (M1 w) !ix = M1 (gcontainerProductIndex (proxy# :: Proxy# es) (proxy# :: Proxy# m) w ix)
  gcontainerProductReplicate _ _ !len (M1 a) = fmap M1 (gcontainerProductReplicate (proxy# :: Proxy# es) (proxy# :: Proxy# v) len a)
  -- gcontainerProductWrite _ (M1 w) ix (M1 e) = gcontainerProductWrite (Proxy :: Proxy v) w ix e

instance forall (e :: Elem) (es :: [Elem]) (v1 :: Type -> Type) (v2 :: Type -> Type) (m1 :: Type -> Type) (m2 :: Type -> Type) (e1 :: Type -> Type) (e2 :: Type -> Type).
       (GContainerProduct '[e] v1 m1 e1, GContainerProduct es v2 m2 e2)
    => GContainerProduct (e ': es) (v1 :*: v2) (m1 :*: m2) (e1 :*: e2) where
  {-# INLINE gcontainerProductIndex #-}
  gcontainerProductIndex _ _ (w1 :*: w2) ix = gcontainerProductIndex (proxy# :: Proxy# '[e]) (proxy# :: Proxy# m1) w1 ix :*: gcontainerProductIndex (proxy# :: Proxy# es) (proxy# :: Proxy# m2) w2 ix
  gcontainerProductReplicate _ _ !len (e1 :*: e2) = liftA2 (:*:)
    (gcontainerProductReplicate (proxy# :: Proxy# '[e]) (proxy# :: Proxy# v1) len e1)
    (gcontainerProductReplicate (proxy# :: Proxy# es) (proxy# :: Proxy# v2) len e2)
  -- gcontainerProductWrite _ (w1 :*: w2) ix (e1 :*: e2) = do
  --   gcontainerProductWrite (Proxy :: Proxy v1) w1 ix e1
  --   gcontainerProductWrite (Proxy :: Proxy v2) w2 ix e2

instance forall (a :: Type) (v :: Nat -> Type -> Type) (m :: Type -> Nat -> Type -> Type) h i j. (m ~ Mutable v, PolyContainer v) => GContainerProduct '[ 'Poly] (K1 j (v 42 a)) (K1 h (m RealWorld 42 a)) (K1 i a) where
  {-# INLINE gcontainerProductIndex #-}
  gcontainerProductIndex _ _ (K1 !w) (U.Index !ix) = K1 (indexPoly w (U.Index ix))
  gcontainerProductReplicate _ _ (U.Length !len) (K1 e) = fmap K1 (replicatePoly (U.Length len) e)
  -- gcontainerProductWrite _ (Rec1 w) ix (K1 e) = write w ix e

instance forall (v :: Nat -> Type) (m :: Type -> Nat -> Type) (e :: Type) h i j. (m ~ Mutable v, Container v, Element v ~ e) => GContainerProduct '[ 'Mono] (K1 j (v 42)) (K1 h (m RealWorld 42)) (K1 i e) where
  {-# INLINE gcontainerProductIndex #-}
  gcontainerProductIndex _ _ (K1 !w) (U.Index ix) = K1 (index w (U.Index ix))
  gcontainerProductReplicate _ _ (U.Length !len) (K1 e) = fmap K1 (replicate (U.Length len) e)

type instance Mutable A.Vector = A.MutableVector

instance PolyContainer A.Vector where
  indexPoly = A.index
  replicatePoly = A.replicate

