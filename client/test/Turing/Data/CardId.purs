module Test.Turing.Data.CardId where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Unit.Main (runTest)
import Test.Unit
import Test.Unit.Assert
import Turing.Data.CardId
import Data.Newtype
import Data.Show

main :: Effect Unit
main = runTest do
    suite "CardId" do
        test "show" do
            equal "A" (show (CardId "A"))
