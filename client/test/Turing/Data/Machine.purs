module Test.Turing.Data.Machine (main) where

import Prelude
import Control.Monad.Free (Free)
import Effect (Effect)
import Test.QuickCheck (Result, (===))
import Test.Unit (TestF, suite, test, testSkip)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Turing.Data.Machine as Machine
import Turing.Optics
import Data.Maybe
import Data.Traversable
import Effect.Class.Console

main :: Effect Unit
main = runTest do
    suite "Machine" do
        testSubStep

testSubStep :: Free TestF Unit
testSubStep = do
    suite "subStep" do
        let
            machine =
                Machine.empty
                    # set (_tape <<< at 0 <<< _Just) false
                    # set (_card <<< _falseInstruction <<< _output) true
                    # set (_card <<< _falseInstruction <<< _moveRight) true
                    # set (_card <<< _trueInstruction <<< _output) false
                    # set (_card <<< _trueInstruction <<< _moveRight) false

        test "advance from read" do
            machine
                # set _subState Machine.Read
                # Machine.subStep
                # view _subState
                # Assert.equal Machine.Write

        test "advance from write" do
            let
                m =
                    machine
                        # set _subState Machine.Write
                        # Machine.subStep

            Assert.equal (Machine.Move false) (m.subState)
            Assert.equal (Just true) (view (at m.position) m.tape)

        testSkip "advance from move" do
            Machine.empty
                # set _subState (Machine.Move false)
                # Machine.subStep
                # flap
                    [ view _subState >>> Assert.equal (Machine.ChangeCard false)
                    , view _position >>> Assert.equal 1
                    ]
                # sequence_

        testSkip "advance from change card" do
            Machine.empty
                # set _subState (Machine.ChangeCard false)
                # Machine.subStep
                # view _subState
                # Assert.equal Machine.Read


