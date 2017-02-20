module Data.Complex
  ( Complex(..)
  , real
  , imag
  ) where

import Prelude

data Complex = Complex Number Number

derive instance eqComplex :: Eq Complex

instance semiringComplex :: Semiring Complex where
  zero = Complex 0.0 0.0
  one  = Complex 1.0 0.0
  add (Complex real1 imag1) (Complex real2 imag2) =
    Complex (real1 + real2) (imag1 + imag2)
  mul (Complex real1 imag1) (Complex real2 imag2) =
    Complex (real1 * real2 - imag1 * imag2)
            (real1 * imag2 + imag1 * real2)

instance ringComplex :: Ring Complex where
  sub (Complex real1 imag1) (Complex real2 imag2) =
    Complex (real1 - real2) (imag1 - imag2)

instance showComplex :: Show Complex where
  show (Complex real' imag') =
    "(Complex " <> show real' <> " " <> show imag' <> ")"

-- | Extract the real part of a complex number.
real :: Complex -> Number
real (Complex real' _) = real'

-- | Extract the imaginary part of a complex number.
imag :: Complex -> Number
imag (Complex _ imag') = imag'
