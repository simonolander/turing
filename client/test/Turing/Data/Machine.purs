module Test.Turing.Data.Machine (main) where

import Prelude
import Control.Monad.Free (Free)
import Effect (Effect)
import Test.QuickCheck (Result, (===))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Turing.Data.Machine as Machine

main :: Effect Unit
main = runTest do
    suite "Machine" do
        testAdvanceSubState

testAdvanceSubState :: Free TestF Unit
testAdvanceSubState = do
    suite "advanceSubState" do
        test "advance from read" do
            Machine.empty
                # Machine.advanceSubState
                # Machine.getSubState
                # Assert.equal Machine.Write

