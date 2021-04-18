module Test.Turing.Utils.Random where

import Prelude (Unit, bind, pure, ($), (==))

import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Unit.Main (runTest)
import Test.Unit
import Turing.Utils.Random
import Test.Unit.QuickCheck (quickCheck)
import Data.String (length)
import Effect.Unsafe (unsafePerformEffect)
import Test.Unit.Assert (equal)

main :: Effect Unit
main = runTest do
    suite "Random" do
        test "random string length" do
            string <- liftEffect $ randomString 6
            equal 6 $ length string
