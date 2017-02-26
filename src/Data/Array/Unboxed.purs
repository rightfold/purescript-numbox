module Data.Array.Unboxed
  ( UnboxedArray

  , build
  , fill

  , foldl
  , foldr

  , index
  , (!!)
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, pureST)
import Data.Array.Unboxed.ST (class STUnboxedArray)
import Data.Array.Unboxed.ST as ST
import Data.Function.Uncurried (Fn2)
import Data.Maybe (Maybe)
import Prelude
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

-- | An immutable array with unboxed elements.
foreign import data UnboxedArray :: (Type -> Type) -> Type -> Type

--------------------------------------------------------------------------------

-- | Create a new immutable array from a mutable array. The conversion is
-- | instantanious.
build
  :: ∀ as a
   . (STUnboxedArray as a)
  => (∀ r e. Eff (st :: ST r | e) (as r))
  -> UnboxedArray as a
build go = pureST do
  array <- go
  pure $ (unsafeCoerce :: ∀ r. as r -> UnboxedArray as a) array

-- | *O(n)* Create a new array of a certain length with all elements set to a
-- | certain value.
fill :: ∀ as a. (STUnboxedArray as a) => Int -> a -> UnboxedArray as a
fill length value = build (ST.new length value)

--------------------------------------------------------------------------------

foldl :: ∀ as a z. (STUnboxedArray as a) => (z -> a -> z) -> z -> UnboxedArray as a -> z
foldl f z xs = foldl' ST.unsafePeek (ST.length xs') f z xs'
  where xs' = (unsafeCoerce :: UnboxedArray as a -> as _) xs

foreign import foldl'
  :: ∀ as a r e z
   . Fn2 Int (as r) (Eff (st :: ST r | e) a)
  -> Int
  -> (z -> a -> z)
  -> z
  -> as r
  -> z

foldr :: ∀ as a z. (STUnboxedArray as a) => (a -> z -> z) -> z -> UnboxedArray as a -> z
foldr f z xs = foldr' ST.unsafePeek (ST.length xs') f z xs'
  where xs' = (unsafeCoerce :: UnboxedArray as a -> as _) xs

foreign import foldr'
  :: ∀ as a r e z
   . Fn2 Int (as r) (Eff (st :: ST r | e) a)
  -> Int
  -> (a -> z -> z)
  -> z
  -> as r
  -> z

--------------------------------------------------------------------------------

-- | *O(1)* Get the element at the specified index.
index :: ∀ as a. (STUnboxedArray as a) => UnboxedArray as a -> Int -> Maybe a
index array offset = pureST do
  ST.peek offset $ (unsafeCoerce :: ∀ r. UnboxedArray as a -> as r) array

infixl 8 index as !!
