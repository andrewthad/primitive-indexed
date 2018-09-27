{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Vector.Inductive.Boxed
  ( BoxedInductiveVector(..)
  , traverse_
  , traverseReflected_
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Nat,type (+))
import Vector.Unsafe (Index(..),Length(..))

data BoxedInductiveVector :: Nat -> Type -> Type where
  BoxedInductiveVectorCons ::
    a -> BoxedInductiveVector n a -> BoxedInductiveVector (n + 1) a
  BoxedInductiveVectorNil :: BoxedInductiveVector 0 a

traverse_ :: forall n m a b. Applicative m
  => (Index n -> a -> m b)
  -> BoxedInductiveVector n a
  -> m ()
traverse_ f = go 0 where
  go :: forall i. Int -> BoxedInductiveVector i a -> m ()
  go !_ BoxedInductiveVectorNil = pure ()
  go !ix (BoxedInductiveVectorCons x xs) = f (Index ix) x *> go (ix + 1) xs

traverseReflected_ :: forall n m a b. Applicative m
  => (Index n -> a -> m b)
  -> Length n
  -> BoxedInductiveVector n a
  -> m ()
traverseReflected_ f (Length n) = go (n - 1) where
  go :: forall i. Int -> BoxedInductiveVector i a -> m ()
  go !_ BoxedInductiveVectorNil = pure ()
  go !ix (BoxedInductiveVectorCons x xs) = f (Index ix) x *> go (ix - 1) xs

