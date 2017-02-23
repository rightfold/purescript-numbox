-- | Mutable arrays with unboxed elements.
module Data.Array.Unboxed.ST
  ( class STUnboxedArray
  , unsafeNew
  , length
  , unsafePeek
  , unsafePoke
  , new
  , peek
  , poke

  , STUnboxedArraySlice
  , slice

  , STUnboxedInt32Array

  , STUnboxedFloat64Array

  , STUnboxedComplex128Array

  , STUnboxedTupleArray
  , zip
  , unzip

  , lockstep
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.Complex (Complex(..))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, mkFn2, mkFn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude

--------------------------------------------------------------------------------

class STUnboxedArray as a | as -> a where
  unsafeNew :: ∀ r e. Fn2 Int a (Eff (st :: ST r | e) (as r))
  length :: ∀ r. Fn1 (as r) Int
  unsafePeek :: ∀ r e. Fn2 Int (as r) (Eff (st :: ST r | e) a)
  unsafePoke :: ∀ r e. Fn3 Int a (as r) (Eff (st :: ST r | e) Unit)

new
  :: ∀ as a r e
   . (STUnboxedArray as a)
  => Int
  -> a
  -> Eff (st :: ST r | e) (as r)
new length' value = runFn2 unsafeNew (length' `max` 0) value

-- | *O(1)* Get the element at the offset in the array.
peek
  :: ∀ as a r e
   . (STUnboxedArray as a)
  => Int
  -> as r
  -> Eff (st :: ST r | e) (Maybe a)
peek index array
  | index >= length array = pure Nothing
  | otherwise = do
      value <- runFn2 unsafePeek index array
      pure $ Just value

-- | *O(1)* Store the value at the offset in the array. No-op if the offset is
-- | out of bounds.
poke
  :: ∀ as a r e
   . (STUnboxedArray as a)
  => Int
  -> a
  -> as r
  -> Eff (st :: ST r | e) Unit
poke index value array
  | index >= length array = pure unit
  | otherwise = runFn3 unsafePoke index value array

--------------------------------------------------------------------------------

data STUnboxedArraySlice as r = STUnboxedArraySlice Int Int (as r)

-- | *O(1)* Slice an array. Mutating the slice mutates the original and vice
-- | versa. The first integer argument is the slice offset and the second
-- | integer argument is the slice length.
slice
  :: ∀ as a r
   . (STUnboxedArray as a)
  => Int
  -> Int
  -> as r
  -> Maybe (STUnboxedArraySlice as r)
slice start length' base
  | length base - (length' `max` 0) - start > 0 =
      Just (STUnboxedArraySlice start (length' `max` 0) base)
  | otherwise = Nothing

instance stUnboxedArraySTUnboxedArraySlice :: (STUnboxedArray as a) => STUnboxedArray (STUnboxedArraySlice as) a where
  unsafeNew = mkFn2 \length' value -> do
    array <- runFn2 unsafeNew length' value
    pure $ STUnboxedArraySlice 0 length' array
  length (STUnboxedArraySlice _ length' _) =
    length'
  unsafePeek = mkFn2 \index (STUnboxedArraySlice start _ base) ->
    runFn2 unsafePeek (start + index) base
  unsafePoke = mkFn3 \index value (STUnboxedArraySlice start _ base) ->
    runFn3 unsafePoke (start + index) value base

--------------------------------------------------------------------------------

-- | An array of unboxed 32-bit signed integers.
foreign import data STUnboxedInt32Array :: Type -> Type

instance stUnboxedArraySTUnboxedInt32Array :: STUnboxedArray STUnboxedInt32Array Int where
  unsafeNew = unsafeNewSTUnboxedInt32Array
  length = lengthSTUnboxedInt32Array
  unsafePeek = unsafePeekSTUnboxedInt32Array
  unsafePoke = unsafePokeSTUnboxedInt32Array

foreign import unsafeNewSTUnboxedInt32Array :: ∀ r e. Fn2 Int Int (Eff (st :: ST r | e) (STUnboxedInt32Array r))
foreign import lengthSTUnboxedInt32Array :: ∀ r. Fn1 (STUnboxedInt32Array r) Int
foreign import unsafePeekSTUnboxedInt32Array :: ∀ r e. Fn2 Int (STUnboxedInt32Array r) (Eff (st :: ST r | e) Int)
foreign import unsafePokeSTUnboxedInt32Array :: ∀ r e. Fn3 Int Int (STUnboxedInt32Array r) (Eff (st :: ST r | e) Unit)

--------------------------------------------------------------------------------

-- | An array of unboxed 64-bit floating point numbers.
foreign import data STUnboxedFloat64Array :: Type -> Type

instance stUnboxedArraySTUnboxedFloat64Array :: STUnboxedArray STUnboxedFloat64Array Number where
  unsafeNew = unsafeNewSTUnboxedFloat64Array
  length = lengthSTUnboxedFloat64Array
  unsafePeek = unsafePeekSTUnboxedFloat64Array
  unsafePoke = unsafePokeSTUnboxedFloat64Array

foreign import unsafeNewSTUnboxedFloat64Array :: ∀ r e. Fn2 Int Number (Eff (st :: ST r | e) (STUnboxedFloat64Array r))
foreign import lengthSTUnboxedFloat64Array :: ∀ r. Fn1 (STUnboxedFloat64Array r) Int
foreign import unsafePeekSTUnboxedFloat64Array :: ∀ r e. Fn2 Int (STUnboxedFloat64Array r) (Eff (st :: ST r | e) Number)
foreign import unsafePokeSTUnboxedFloat64Array :: ∀ r e. Fn3 Int Number (STUnboxedFloat64Array r) (Eff (st :: ST r | e) Unit)

--------------------------------------------------------------------------------

-- | An array of unboxed 128-bit complex floating point numbers.
newtype STUnboxedComplex128Array r =
  STUnboxedComplex128Array (STUnboxedFloat64Array r)

instance stUnboxedArraySTUnboxedComplex128Array :: STUnboxedArray STUnboxedComplex128Array Complex where
  unsafeNew = mkFn2 \length' (Complex real imag) ->
    if real == 0.0 && imag == 0.0
      then do base <- runFn2 unsafeNew (length' * 2) 0.0
              pure $ STUnboxedComplex128Array base
      else do base <- runFn2 unsafeNew (length' * 2) real
              runFn3 fillImag length' imag base
              pure $ STUnboxedComplex128Array base
  length (STUnboxedComplex128Array base) =
    length base / 2
  unsafePeek = mkFn2 \index (STUnboxedComplex128Array base) -> do
    real <- runFn2 unsafePeek (index * 2 + 0) base
    imag <- runFn2 unsafePeek (index * 2 + 1) base
    pure $ Complex real imag
  unsafePoke = mkFn3 \index (Complex real imag) (STUnboxedComplex128Array base) -> do
    runFn3 unsafePoke (index * 2 + 0) real base
    runFn3 unsafePoke (index * 2 + 1) imag base

foreign import fillImag :: ∀ r e. Fn3 Int Number (STUnboxedFloat64Array r) (Eff (st :: ST r | e) Unit)

--------------------------------------------------------------------------------

-- | A pair of arrays of unboxed elements, with a tuple element interface.
data STUnboxedTupleArray as bs a b r = STUnboxedTupleArray (as r) (bs r)

instance stUnboxedArraySTUnboxedTupleArray :: (STUnboxedArray as a, STUnboxedArray bs b) => STUnboxedArray (STUnboxedTupleArray as bs a b) (Tuple a b) where
  unsafeNew = mkFn2 \length' (Tuple a b) -> do
    as <- runFn2 unsafeNew length' a
    bs <- runFn2 unsafeNew length' b
    pure $ STUnboxedTupleArray as bs
  length (STUnboxedTupleArray as _) =
    length as
  unsafePeek = mkFn2 \index (STUnboxedTupleArray as bs) -> do
    a <- runFn2 unsafePeek index as
    b <- runFn2 unsafePeek index bs
    pure $ Tuple a b
  unsafePoke = mkFn3 \index (Tuple a b) (STUnboxedTupleArray as bs) -> do
    runFn3 unsafePoke index a as
    runFn3 unsafePoke index b bs

-- | *O(1)* Zip two arrays. Mutating the tuple array mutates the original
-- | arrays and vice versa. The arrays must have the same length.
zip
  :: ∀ as bs a b r
   . ( STUnboxedArray as a
     , STUnboxedArray bs b
     )
  => as r
  -> bs r
  -> Maybe (STUnboxedTupleArray as bs a b r)
zip as bs
  | length as == length bs = Just $ STUnboxedTupleArray as bs
  | otherwise = Nothing

-- | *O(1)* Unzip an array of tuples. Mutating the arrays mutates the original
-- | array and vice versa.
unzip
  :: ∀ as bs a b r
   . STUnboxedTupleArray as bs a b r
  -> Tuple (as r) (bs r)
unzip (STUnboxedTupleArray as bs) = Tuple as bs

--------------------------------------------------------------------------------

-- | *Ω(min(n, m, l))* Perform an operation on multiple arrays at once.
lockstep
  :: ∀ as a bs b cs c r e
   . ( STUnboxedArray as a
     , STUnboxedArray bs b
     , STUnboxedArray cs c
     )
  => (a -> b -> c)
  -> as r
  -> bs r
  -> cs r
  -> Eff (st :: ST r | e) Unit
lockstep f as bs cs =
  lockstep' unsafePeek unsafePeek unsafePoke f as bs cs
            (length as `min` length bs `min` length cs)

foreign import lockstep'
  :: ∀ as bs cs a b c r e
   . Fn2 Int as (Eff (st :: ST r | e) a)
  -> Fn2 Int bs (Eff (st :: ST r | e) b)
  -> Fn3 Int c cs (Eff (st :: ST r | e) Unit)
  -> (a -> b -> c)
  -> as
  -> bs
  -> cs
  -> Int
  -> Eff (st :: ST r | e) Unit
