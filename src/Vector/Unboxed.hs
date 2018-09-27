{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall #-}

module Vector.Unboxed
  ( -- * Types
    UnboxedVector
  , MutableUnboxedVector
    -- * Primops
  , new
  , replicate
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

import Prelude hiding (read,length,zipWith,reverse,replicate)
import Control.Monad.Primitive (PrimMonad,PrimState)
import Data.Primitive.Types (Prim)
import Data.Primitive.PrimArray
import Vector.Unsafe (UnboxedVector(..),MutableUnboxedVector(..),Index(..),Length(..))

new :: (PrimMonad m, Prim a) => Length n -> m (MutableUnboxedVector (PrimState m) n a)
{-# INLINE new #-}
new (Length n) = fmap MutableUnboxedVector (newPrimArray n)

replicate :: (PrimMonad m, Prim a) => Length n -> a -> m (MutableUnboxedVector (PrimState m) n a)
{-# INLINE replicate #-}
replicate (Length n) a = fmap MutableUnboxedVector $ do
  x <- newPrimArray n
  setPrimArray x 0 n a
  return x

index :: Prim a => UnboxedVector n a -> Index n -> a
{-# INLINE index #-}
index (UnboxedVector arr) (Index i) = indexPrimArray arr i

read :: (PrimMonad m, Prim a) => MutableUnboxedVector (PrimState m) n a -> Index n -> m a
{-# INLINE read #-}
read (MutableUnboxedVector marr) (Index i) = readPrimArray marr i

write :: (PrimMonad m, Prim a) => MutableUnboxedVector (PrimState m) n a -> Index n -> a -> m ()
{-# INLINE write #-}
write (MutableUnboxedVector marr) (Index i) a = writePrimArray marr i a

-- | Get the length of a vector.
length :: Prim a => UnboxedVector n a -> Length n
{-# INLINE length #-}
length (UnboxedVector arr) = Length (sizeofPrimArray arr)

-- | Get the length of a mutable vector.
size :: Prim a => MutableUnboxedVector s n a -> Length n
{-# INLINE size #-}
size (MutableUnboxedVector marr) = Length (sizeofMutablePrimArray marr)

with :: PrimArray a -> (forall n. UnboxedVector n a -> b) -> b
{-# INLINE with #-}
with arr f = f (UnboxedVector arr)

forget :: UnboxedVector n a -> PrimArray a
{-# INLINE forget #-}
forget (UnboxedVector arr) = arr

-- | Freeze the mutable vector. The argument must not be reused after
-- this function is called on it. 
unsafeFreeze :: PrimMonad m => MutableUnboxedVector (PrimState m) n a -> m (UnboxedVector n a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze (MutableUnboxedVector marr) = fmap UnboxedVector (unsafeFreezePrimArray marr)

