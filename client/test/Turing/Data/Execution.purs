module Test.Turing.Data.Execution where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Unit.Main (runTest)
import Test.Unit
import Test.Unit.Assert
import Turing.Data.Execution
import Turing.Data.ExecutionState
import Turing.Data.Program as Prog
import Turing.Data.Card as Card
import Turing.Data.Tape as Tape
import Data.Newtype
import Data.Show (show)
import Test.Unit.QuickCheck

main :: Effect Unit
main = runTest do
    suite "Execution" do
        let
            execution :: Execution
            execution =
                { program: Prog.program Card.empty
                , tape: Tape.empty
                , position: 0
                , card: Card.empty
                , state: Scanning
                }

        suite "sub step" do
            test "scanning" do
                equal (Writing false) (subStep execution).state

