module Test.Data.Signal.Decomposition
  ( main
  ) where

import Control.Monad.Eff.Class (liftEff)
import Data.Array.Unboxed.ST (new, peek, poke)
import Data.Complex (Complex(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.Signal.Decomposition (fft64)
import Prelude
import Test.Unit (test)
import Test.Unit.Assert as Assert

main = test "Data.Signal.Decomposition" do
  input  <- liftEff $ new 8 0.0
  output <- liftEff $ new 8 (Complex 0.0 0.0)

  liftEff $ poke 0 1.0 input
  liftEff $ poke 1 1.0 input
  liftEff $ poke 2 1.0 input
  liftEff $ poke 3 1.0 input
  liftEff $ poke 4 0.0 input
  liftEff $ poke 5 0.0 input
  liftEff $ poke 6 0.0 input
  liftEff $ poke 7 0.0 input

  liftEff $ fft64 input output

  let check real imag index = do
        entry <- liftEff (peek index output)
        let msg = "expected " <> show (Just (Complex real imag))
                      <> ", got " <> show entry
        Assert.assert msg (fromMaybe false $ approx (Complex real imag) <$> entry)
  check 4.0 0.0       0
  check 1.0 (-2.4142) 1
  check 0.0 0.0       2
  check 1.0 (-0.4142) 3
  check 0.0 0.0       4
  check 1.0 0.4142    5
  check 0.0 0.0       6
  check 1.0 2.4142    7

  liftEff $ fft64 input output

approx :: Complex -> Complex -> Boolean
approx (Complex real1 imag1) (Complex real2 imag2) =
  abs (real1 - real2) < ε
  && abs (imag1 - imag2) < ε
  where ε = 1e-4
