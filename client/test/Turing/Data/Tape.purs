module Test.Turing.Data.Tape where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Unit.Main (runTest)
import Test.Unit
import Test.Unit.Assert
import Turing.Data.Tape
import Data.Newtype
import Data.Show (show)
import Test.Unit.QuickCheck

main :: Effect Unit
main = runTest do
    suite "Tape" do
        test "scan empty" do
            quickCheck \position -> scan position empty == false
        test "write" do
            quickCheck
                \position symbol ->
                    let
                        tape = write position symbol empty
                    in
                    scan position tape == symbol

