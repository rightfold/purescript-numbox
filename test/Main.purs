module Test.Main
  ( main
  ) where

import Prelude
import Test.Data.Array.Unboxed.ST as Data.Array.Unboxed.ST
import Test.Data.FFT as Data.FFT
import Test.Unit.Main (runTest)

main = runTest do
  Data.Array.Unboxed.ST.main
  Data.FFT.main
