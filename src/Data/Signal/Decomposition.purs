-- | Signal decomposition functions.
module Data.Signal.Decomposition
  ( fft64
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.Array.Unboxed.ST (STUnboxedComplex128Array, STUnboxedFloat64Array)
import Prelude

-- | *Θ(n log n)* Fast Fourier transform on 64-bit floating point numbers. The
-- | array lengths must be equal and a power of two. If they are not, the
-- | behavior is undefined.
foreign import fft64
  :: ∀ r e
   . STUnboxedFloat64Array r
  -> STUnboxedComplex128Array r
  -> Eff (st :: ST r | e) Unit
