{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -O2 #-}

module Data.Primitive.Indexed.Array
  ( -- * Types
    Vector
  , MutableVector
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
    -- * Functions
  , zipWith
  , reverse
  ) where

import Prelude hiding (read,length,zipWith,reverse)
import Control.Monad.Primitive (PrimMonad,PrimState)
import Control.Monad.ST (runST)
import Data.Primitive.Array
import Data.Primitive.Indexed.Unsafe (Vector(..),MutableVector(..),Index(..),Length(..))
import Data.Primitive.Indexed.Types (ascendM,reflect)

new :: PrimMonad m => Length n -> a -> m (MutableVector n (PrimState m) a)
{-# INLINE new #-}
new (Length n) a = fmap MutableVector (newArray n a)

index :: Vector n a -> Index n -> a
{-# INLINE index #-}
index (Vector arr) (Index i) = indexArray arr i

read :: PrimMonad m => MutableVector n (PrimState m) a -> Index n -> m a
{-# INLINE read #-}
read (MutableVector marr) (Index i) = readArray marr i

write :: PrimMonad m => MutableVector n (PrimState m) a -> Index n -> a -> m ()
{-# INLINE write #-}
write (MutableVector marr) (Index i) a = writeArray marr i a

-- | Get the length of a vector.
length :: Vector n a -> Length n
{-# INLINE length #-}
length (Vector arr) = Length (sizeofArray arr)

-- | Get the length of a mutable vector.
size :: MutableVector n s a -> Length n
{-# INLINE size #-}
size (MutableVector marr) = Length (sizeofMutableArray marr)

with :: Array a -> (forall n. Vector n a -> b) -> b
{-# INLINE with #-}
with arr f = f (Vector arr)

forget :: Vector n a -> Array a
{-# INLINE forget #-}
forget (Vector arr) = arr

-- | Freeze the mutable vector. The argument must not be reused after
-- this function is called on it. 
unsafeFreeze :: PrimMonad m => MutableVector n (PrimState m) a -> m (Vector n a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze (MutableVector marr) = fmap Vector (unsafeFreezeArray marr)

-- | Zip two vectors of equal length together by applying the given function.
zipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith f v1 v2 = runST $ do
  let !sz = length v1
  mvec <- new sz errorThunk
  ascendM (\ix -> let !c = f (index v1 ix) (index v2 ix) in write mvec ix c) sz
  unsafeFreeze mvec

-- | Reverse a vector
reverse :: Vector n a -> Vector n a
reverse v = runST $ do
  let !sz = length v
  mvec <- new sz errorThunk
  ascendM (\ix -> let !a = index v ix in write mvec (reflect sz ix) a) sz
  unsafeFreeze mvec
  

errorThunk :: a
{-# NOINLINE errorThunk #-}
errorThunk = error "Data.Primitive.Indexed.Array: uninitialized element"

