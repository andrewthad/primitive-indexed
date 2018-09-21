{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Arrays of boxed elements. This mimics the API of @Data.Primitive.Array@.
-- However, all functions that deal with indices are total and cannot cause
-- runtime crashes.
module Data.Primitive.Indexed.Array
  ( -- * Types
    Vector
  , MutableVector
    -- * Primops
  , new
  , replicate
  , index
  , read
  , write
  , length
  , size
  , thaw
  , unsafeFreeze
    -- * Array Interop
  , forget
  , with
    -- * Functions
    -- $functions
  , zipWith
  , reverse
  , update
  , accumulate
  , backpermute
  ) where

import Prelude hiding (read,length,zipWith,reverse,replicate)
import Control.Monad.Primitive (PrimMonad,PrimState)
import Control.Monad.ST (runST)
import Data.Primitive.Array
import Data.Primitive.Indexed.Unsafe (Vector(..),MutableVector(..),Index(..),Length(..))
import Data.Primitive.Indexed.Types (ascendM,reflect,PrimVector)

import qualified Data.Primitive.Indexed.PrimArray as PV

new :: PrimMonad m => Length n -> m (MutableVector (PrimState m) n a)
{-# INLINE new #-}
new n = replicate n errorThunk

replicate :: PrimMonad m => Length n -> a -> m (MutableVector (PrimState m) n a)
{-# INLINE replicate #-}
replicate (Length n) a = fmap MutableVector (newArray n a)

-- | Index into a vector. Use this with functions from "Data.Primitive.Index.Types"
-- to get a hold of indices that can be used.
--
-- >>> let cities = ["Cairo","Atlanta","Tokyo"] :: Array String
-- >>> with cities $ \v -> ascendM (\ix -> putStrLn ("I ❤️ " ++ index v ix)) (length v)
-- I ❤️ Cairo
-- I ❤️ Atlanta
-- I ❤️ Tokyo
index :: Vector n a -> Index n -> a
{-# INLINE index #-}
index (Vector arr) (Index i) = indexArray arr i

read :: PrimMonad m => MutableVector (PrimState m) n a -> Index n -> m a
{-# INLINE read #-}
read (MutableVector marr) (Index i) = readArray marr i

write :: PrimMonad m => MutableVector (PrimState m) n a -> Index n -> a -> m ()
{-# INLINE write #-}
write (MutableVector marr) (Index i) a = writeArray marr i a

-- | Get the length of a vector.
length :: Vector n a -> Length n
{-# INLINE length #-}
length (Vector arr) = Length (sizeofArray arr)

-- | Get the length of a mutable vector.
size :: MutableVector s n a -> Length n
{-# INLINE size #-}
size (MutableVector marr) = Length (sizeofMutableArray marr)

-- | Pass an unindexed array to a function that operates on
-- a vector of any length.
with :: Array a -> (forall n. Vector n a -> b) -> b
{-# INLINE with #-}
with arr f = f (Vector arr)

-- | Discard the phantom length associated with an indexed vector.
forget :: Vector n a -> Array a
{-# INLINE forget #-}
forget (Vector arr) = arr

-- | Create a mutable array from a slice of an immutable array.
--
-- This operation makes a copy of the immutable array, so it is safe to use the
-- immutable array afterward.
thaw :: PrimMonad m => Vector n a -> m (MutableVector (PrimState m) n a)
{-# INLINE thaw #-}
thaw (Vector arr) = fmap MutableVector (thawArray arr 0 (sizeofArray arr))

-- | Freeze the mutable vector. The argument must not be reused after
-- this function is called on it. 
unsafeFreeze :: PrimMonad m => MutableVector (PrimState m) n a -> m (Vector n a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze (MutableVector marr) = fmap Vector (unsafeFreezeArray marr)

-- | Zip two vectors of equal length together by applying the given function.
zipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith f v1 v2 = runST $ do
  let !sz = length v1
  mvec <- new sz
  ascendM (\ix -> let !c = f (index v1 ix) (index v2 ix) in write mvec ix c) sz
  unsafeFreeze mvec

-- | Reverse a vector
reverse :: Vector n a -> Vector n a
reverse v = runST $ do
  let !sz = length v
  mvec <- new sz
  ascendM (\ix -> let !a = index v ix in write mvec (reflect sz ix) a) sz
  unsafeFreeze mvec
  
-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @'index' xs i@.
backpermute :: Vector n a -> PrimVector m (Index n) -> Vector m a
backpermute v ixs = runST $ do
  let !sz = PV.length ixs
  mvec <- new sz
  ascendM (\ix -> let !a = index v (PV.index ixs ix) in write mvec ix a) sz
  unsafeFreeze mvec

-- | /O(m+n)/ For each pair @(i,a)@ from the index/value pair vectors,
-- replace the vector element at position @i@ by @a@.
update :: 
     Vector n a -- ^ initial vector (of length @n@)
  -> PrimVector m (Index n) -- ^ vector of indices
  -> Vector m a -- ^ vector of values
  -> Vector n a
update v ixs vals = runST $ do
  mvec <- thaw v
  let !sz = length vals
  ascendM (\ix -> let !a = index vals ix in write mvec (PV.index ixs ix) a) sz
  unsafeFreeze mvec

-- | /O(m+n)/ For each pair @(i,a)@ from the index/value pair vectors,
-- replace the vector element at position @i@ by @f a b@.
accumulate :: 
     (a -> b -> a) -- ^ accumulating function @f@
  -> Vector n a -- ^ initial vector (of length @n@)
  -> PrimVector m (Index n) -- ^ vector of indices
  -> Vector m b -- ^ vector of values
  -> Vector n a
accumulate f v ixs vals = runST $ do
  mvec <- thaw v
  let !sz = length vals
  ascendM
    (\ix -> do
      let !ixA = PV.index ixs ix
      a <- read mvec ixA
      write mvec ixA (f a (index vals ix))
    ) sz
  unsafeFreeze mvec

errorThunk :: a
{-# NOINLINE errorThunk #-}
errorThunk = error "Data.Primitive.Indexed.Array: uninitialized element"

{- $functions

All of these can be safely written using the other primitives provided
by this module. They are provided here to demonstrate how to write index-safe
variants of functions from the @vector@ library without sacrificing performance.

-}

{- $setup
 
These are run before each GHCi block when doctest runs.

>>> :set -XOverloadedLists

-}
