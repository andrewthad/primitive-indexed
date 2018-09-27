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

module Vector.Generic.Polymorphic
  ( Vector(..)
  ) where

import Control.Monad.ST (ST)
import Data.Kind (Type,Constraint)
import Data.Primitive (Prim)
import GHC.TypeLits (Nat)
import Vector.Types hiding (Element)
import qualified Vector.Boxed as B
import qualified Vector.Unboxed as U

class Vector (v :: Nat -> Type -> Type) where
  type Element v :: Type -> Constraint
  index :: Element v a => v n a -> Index n -> a
  replicate :: Element v a => Length n -> a -> ST s (Mutable v s n a)

class Always a
instance Always a

instance Vector BoxedVector where
  type Element BoxedVector = Always
  index = B.index
  replicate = B.replicate

instance Vector UnboxedVector where
  type Element UnboxedVector = Prim
  index = U.index
  replicate = U.replicate



