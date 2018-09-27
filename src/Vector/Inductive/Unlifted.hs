{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Vector.Inductive.Unlifted
  ( UnliftedInductiveVector(..)
  ) where

import Data.Kind (Type)
import GHC.Exts (TYPE,RuntimeRep(UnliftedRep))
import GHC.TypeLits (Nat,type (+))

data UnliftedInductiveVector :: Nat -> TYPE 'UnliftedRep -> Type where
  UnliftedInductiveVectorCons ::
    a -> UnliftedInductiveVector n a -> UnliftedInductiveVector (n + 1) a
  UnliftedInductiveVectorNil :: UnliftedInductiveVector 0 a

