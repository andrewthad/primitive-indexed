{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Arrays of boxed elements. This mimics the API of @Data.Primitive.Array@.
-- However, all functions that deal with indices are total and cannot cause
-- runtime crashes.
module Vector.Boxed
  ( -- * Types
    BoxedVector
  , MutableBoxedVector
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
  , fromListN
  ) where

import Prelude hiding (read,length,zipWith,reverse,replicate)
import Control.Monad.Primitive (PrimMonad,PrimState)
import Control.Monad.ST (runST)
import Data.Primitive.Array (Array,unsafeFreezeArray,sizeofArray,thawArray)
import Vector.Unsafe (BoxedVector(..),MutableBoxedVector(..),Index(..),Length(..))
import Vector.Index (ascendM,reflect)
import Vector.Types (UnboxedVector)

import qualified Vector.Unboxed as PV
import qualified Vector.Inductive.Boxed as VI

new :: PrimMonad m => Length n -> m (MutableBoxedVector (PrimState m) n a)
{-# INLINE new #-}
new n = replicate n errorThunk

replicate :: PrimMonad m => Length n -> a -> m (MutableBoxedVector (PrimState m) n a)
{-# INLINE replicate #-}
replicate (Length n) a = fmap MutableBoxedVector (newArray n a)

-- | Index into a vector. Use this with functions from "Data.Primitive.Index.Types"
-- to get a hold of indices that can be used.
--
-- >>> let cities = ["Cairo","Atlanta","Tokyo"] :: Array String
-- >>> with cities $ \v -> ascendM (\ix -> putStrLn ("I ❤️ " ++ index v ix)) (length v)
-- I ❤️ Cairo
-- I ❤️ Atlanta
-- I ❤️ Tokyo
index :: BoxedVector n a -> Index n -> a
{-# INLINE index #-}
index (BoxedVector arr) (Index i) = indexArray arr i

read :: PrimMonad m => MutableBoxedVector (PrimState m) n a -> Index n -> m a
{-# INLINE read #-}
read (MutableBoxedVector marr) (Index i) = readArray marr i

write :: PrimMonad m => MutableBoxedVector (PrimState m) n a -> Index n -> a -> m ()
{-# INLINE write #-}
write (MutableBoxedVector marr) (Index i) a = writeArray marr i a

-- | Get the length of a vector.
length :: BoxedVector n a -> Length n
{-# INLINE length #-}
length (BoxedVector arr) = Length (sizeofArray arr)

-- | Get the length of a mutable vector.
size :: MutableBoxedVector s n a -> Length n
{-# INLINE size #-}
size (MutableBoxedVector marr) = Length (sizeofMutableArray marr)

-- | Pass an unindexed array to a function that operates on
-- a vector of any length.
with :: Array a -> (forall n. BoxedVector n a -> b) -> b
{-# INLINE with #-}
with arr f = f (BoxedVector arr)

-- | Discard the phantom length associated with an indexed vector.
forget :: BoxedVector n a -> Array a
{-# INLINE forget #-}
forget (BoxedVector arr) = arr

-- | Create a mutable array from a slice of an immutable array.
--
-- This operation makes a copy of the immutable array, so it is safe to use the
-- immutable array afterward.
thaw :: PrimMonad m => BoxedVector n a -> m (MutableBoxedVector (PrimState m) n a)
{-# INLINE thaw #-}
thaw (BoxedVector arr) = fmap MutableBoxedVector (thawArray arr 0 (sizeofArray arr))

-- | Freeze the mutable vector. The argument must not be reused after
-- this function is called on it. 
unsafeFreeze :: PrimMonad m => MutableBoxedVector (PrimState m) n a -> m (BoxedVector n a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze (MutableBoxedVector marr) = fmap BoxedVector (unsafeFreezeArray marr)

-- | Zip two vectors of equal length together by applying the given function.
zipWith :: (a -> b -> c) -> BoxedVector n a -> BoxedVector n b -> BoxedVector n c
zipWith f v1 v2 = runST $ do
  let !sz = length v1
  mvec <- new sz
  ascendM (\ix -> let !c = f (index v1 ix) (index v2 ix) in write mvec ix c) sz
  unsafeFreeze mvec

-- | Reverse a vector
reverse :: BoxedVector n a -> BoxedVector n a
reverse v = runST $ do
  let !sz = length v
  mvec <- new sz
  ascendM (\ix -> let !a = index v ix in write mvec (reflect sz ix) a) sz
  unsafeFreeze mvec
  
-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @'index' xs i@.
backpermute :: BoxedVector n a -> UnboxedVector m (Index n) -> BoxedVector m a
backpermute v ixs = runST $ do
  let !sz = PV.length ixs
  mvec <- new sz
  ascendM (\ix -> let !a = index v (PV.index ixs ix) in write mvec ix a) sz
  unsafeFreeze mvec

-- | /O(m+n)/ For each pair @(i,a)@ from the index/value pair vectors,
-- replace the vector element at position @i@ by @a@.
update :: 
     BoxedVector n a -- ^ initial vector (of length @n@)
  -> UnboxedVector m (Index n) -- ^ vector of indices
  -> BoxedVector m a -- ^ vector of values
  -> BoxedVector n a
update v ixs vals = runST $ do
  mvec <- thaw v
  let !sz = length vals
  ascendM (\ix -> let !a = index vals ix in write mvec (PV.index ixs ix) a) sz
  unsafeFreeze mvec

-- | /O(m+n)/ For each pair @(i,a)@ from the index/value pair vectors,
-- replace the vector element at position @i@ by @f a b@.
accumulate :: 
     (a -> b -> a) -- ^ accumulating function @f@
  -> BoxedVector n a -- ^ initial vector (of length @n@)
  -> UnboxedVector m (Index n) -- ^ vector of indices
  -> BoxedVector m b -- ^ vector of values
  -> BoxedVector n a
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

fromListN :: Length n -> VI.BoxedInductiveVector n a -> BoxedVector n a
fromListN !sz xs = runST $ do
  m <- new sz
  VI.traverse_ (write m) xs
  unsafeFreeze m

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

