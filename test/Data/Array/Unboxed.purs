module Test.Data.Array.Unboxed
  ( main
  ) where

import Control.Monad.Eff.Class (liftEff)
import Data.Array.Unboxed (UnboxedArray, build, fill, foldl, foldr)
import Data.Array.Unboxed.ST (STUnboxedInt32Array)
import Data.Array.Unboxed.ST as ST
import Prelude
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert

main = suite "Data.Array.Unboxed.ST" do
  test "fill" do
    let xs = fill 10 4 :: UnboxedArray STUnboxedInt32Array Int
    let x = foldl (\a b -> a && b == 4) true xs
    Assert.equal true x

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

  test "foldr" do
    let xs = build do
          (xs' :: STUnboxedInt32Array _) <- ST.new 4 0
          liftEff $ ST.poke 0 1 xs'
          liftEff $ ST.poke 1 2 xs'
          liftEff $ ST.poke 2 3 xs'
          liftEff $ ST.poke 3 4 xs'
          pure xs'
    let x = foldr (-) 0 xs
    Assert.equal x (4 - 3 - 2 - 1 - 0)
