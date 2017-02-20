module Data.Array.Unboxed
  ( UnboxedArray

  , fill

  , index
  , (!!)
  ) where

import Control.Monad.ST (pureST)
import Data.Array.Unboxed.ST (class STUnboxedArray)
import Data.Array.Unboxed.ST as ST
import Data.Maybe (Maybe)
import Prelude
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

-- | An immutable array with unboxed elements.
foreign import data UnboxedArray :: (Type -> Type) -> Type -> Type

--------------------------------------------------------------------------------

-- | *O(n)* Create a new array of a certain length with all elements set to a
-- | certain value.
fill :: ∀ as a. (STUnboxedArray as a) => Int -> a -> UnboxedArray as a
fill length value = pureST do
  array <- ST.new length value
  pure $ (unsafeCoerce :: ∀ r. as r -> UnboxedArray as a) array

--------------------------------------------------------------------------------

-- | *O(1)* Get the element at the specified index.
index :: ∀ as a. (STUnboxedArray as a) => UnboxedArray as a -> Int -> Maybe a
index array offset = pureST do
  ST.peek offset $ (unsafeCoerce :: ∀ r. UnboxedArray as a -> as r) array

infixl 8 index as !!
