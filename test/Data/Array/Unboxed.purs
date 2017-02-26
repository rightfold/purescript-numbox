module Test.Data.Array.Unboxed
  ( main
  ) where

import Control.Monad.Eff.Class (liftEff)
import Data.Array.Unboxed (build, foldl)
import Data.Array.Unboxed.ST (STUnboxedInt32Array)
import Data.Array.Unboxed.ST as ST
import Prelude
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert

main = suite "Data.Array.Unboxed.ST" do
  test "foldl" do
    let xs = build do
          (xs' :: STUnboxedInt32Array _) <- ST.new 4 0
          liftEff $ ST.poke 0 1 xs'
          liftEff $ ST.poke 1 2 xs'
          liftEff $ ST.poke 2 3 xs'
          liftEff $ ST.poke 3 4 xs'
          pure xs'
    let x = foldl (-) 0 xs
    Assert.equal x (0 - 1 - 2 - 3 - 4)
