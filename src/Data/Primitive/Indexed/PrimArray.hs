{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Primitive.Indexed.PrimArray
  ( -- * Types
    PrimVector
  , MutablePrimVector
    -- * Primops
  , new
  , index
  , read
  , write
  , length
  , size
  , unsafeFreeze
    -- * Array Interop
  , forget
  , with
  ) where

import Prelude hiding (read,length,zipWith,reverse)
import Control.Monad.Primitive (PrimMonad,PrimState)
import Data.Primitive.Types (Prim)
import Data.Primitive.PrimArray
import Data.Primitive.Indexed.Unsafe (PrimVector(..),MutablePrimVector(..),Index(..),Length(..))

new :: (PrimMonad m, Prim a) => Length n -> m (MutablePrimVector n (PrimState m) a)
{-# INLINE new #-}
new (Length n) = fmap MutablePrimVector (newPrimArray n)

index :: Prim a => PrimVector n a -> Index n -> a
{-# INLINE index #-}
index (PrimVector arr) (Index i) = indexPrimArray arr i

read :: (PrimMonad m, Prim a) => MutablePrimVector n (PrimState m) a -> Index n -> m a
{-# INLINE read #-}
read (MutablePrimVector marr) (Index i) = readPrimArray marr i

write :: (PrimMonad m, Prim a) => MutablePrimVector n (PrimState m) a -> Index n -> a -> m ()
{-# INLINE write #-}
write (MutablePrimVector marr) (Index i) a = writePrimArray marr i a

-- | Get the length of a vector.
length :: Prim a => PrimVector n a -> Length n
{-# INLINE length #-}
length (PrimVector arr) = Length (sizeofPrimArray arr)

-- | Get the length of a mutable vector.
size :: Prim a => MutablePrimVector n s a -> Length n
{-# INLINE size #-}
size (MutablePrimVector marr) = Length (sizeofMutablePrimArray marr)

with :: PrimArray a -> (forall n. PrimVector n a -> b) -> b
{-# INLINE with #-}
with arr f = f (PrimVector arr)

forget :: PrimVector n a -> PrimArray a
{-# INLINE forget #-}
forget (PrimVector arr) = arr

-- | Freeze the mutable vector. The argument must not be reused after
-- this function is called on it. 
unsafeFreeze :: PrimMonad m => MutablePrimVector n (PrimState m) a -> m (PrimVector n a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze (MutablePrimVector marr) = fmap PrimVector (unsafeFreezePrimArray marr)
