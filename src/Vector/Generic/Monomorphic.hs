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

module Vector.Generic.Monomorphic
  ( Vector(..)
  ) where

import Vector.Types
import Control.Monad.ST (ST)
import Data.Kind (Type)
import GHC.TypeLits (Nat)

class Vector (v :: Nat -> Type) where
  index :: v n -> Index n -> Element v
  replicate :: Length n -> Element v -> ST s (Mutable v s n)

