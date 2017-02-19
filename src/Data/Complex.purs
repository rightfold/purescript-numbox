module Data.Complex
  ( Complex(..)
  ) where

import Prelude

data Complex = Complex Number Number

derive instance eqComplex :: Eq Complex

instance showComplex :: Show Complex where
  show (Complex real imag) =
    "(Complex " <> show real <> " " <> show imag <> ")"
