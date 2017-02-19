module Test.Data.Array.Unboxed.ST
  ( main
  ) where

import Control.Monad.Eff.Class (liftEff)
import Data.Array as Array
import Data.Array.Unboxed.ST (class STUnboxedArray, STUnboxedComplex128Array, STUnboxedFloat64Array, STUnboxedInt32Array, STUnboxedTupleArray, new, peek, poke)
import Data.Complex (Complex(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert

data Proxy1 (a :: Type -> Type) = Proxy1

main = suite "Data.Array.Unboxed.ST" do
  instanceSuite (Proxy1 :: Proxy1 STUnboxedInt32Array)
                "STUnboxedInt32Array"
                1
                (_ + 1)
  instanceSuite (Proxy1 :: Proxy1 STUnboxedFloat64Array)
                "STUnboxedFloat64Array"
                1.0
                (_ + 1.0)
  instanceSuite (Proxy1 :: Proxy1 STUnboxedComplex128Array)
                "STUnboxedComplex128Array"
                (Complex 1.0 2.0)
                (\(Complex real imag) -> Complex (real + 1.0) (imag + 1.0))
  instanceSuite (Proxy1 :: Proxy1 (STUnboxedTupleArray STUnboxedInt32Array STUnboxedFloat64Array Int Number))
                "STUnboxedTupleArray"
                (Tuple 1 2.0)
                (\(Tuple a b) -> Tuple (a + 1) (b + 1.0))
  where
  instanceSuite
    :: ∀ as a
     . ( Eq a
       , Show a
       , STUnboxedArray as a
       )
    => Proxy1 as
    -> String
    -> a
    -> (a -> a)
    -> _
  instanceSuite _ name value transform = suite name do
    let length = 128
    test "peek" do
      (array :: as _) <- liftEff $ new length value
      for_ (Array.range 0 (length - 1)) \i -> do
        value' <- liftEff $ peek i array
        Assert.equal (Just value) value'
    test "poke" do
      (array :: as _) <- liftEff $ new length value
      for_ (Array.range 0 (length - 1)) \i -> do
        liftEff $ poke i (transform value) array
        value' <- liftEff $ peek i array
        Assert.equal (Just (transform value)) value'
