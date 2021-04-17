module Test.Turing.Data.CardId where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Unit.Main (runTest)
import Test.Unit
import Test.Unit.Assert
import Turing.Data.CardId
import Data.Newtype
import Data.Show (show)
import Test.Unit.QuickCheck

main :: Effect Unit
main = runTest do
    suite "CardId" do
        test "show" do
            quickCheck \id -> id == (show (CardId id))
        test "wrap" do
            quickCheck \id -> wrap id == (CardId id)
        test "eq" do
            quickCheck \id1 id2 -> (id1 == id2) == ((CardId id1) == (CardId id2))
