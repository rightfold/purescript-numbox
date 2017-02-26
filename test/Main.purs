module Test.Main
  ( main
  ) where

import Prelude
import Test.Data.Array.Unboxed as Data.Array.Unboxed
import Test.Data.Array.Unboxed.ST as Data.Array.Unboxed.ST
import Test.Data.Signal.Decomposition as Data.Signal.Decomposition
import Test.Unit.Main (runTest)

main = runTest do
  Data.Array.Unboxed.main
  Data.Array.Unboxed.ST.main
  Data.Signal.Decomposition.main
