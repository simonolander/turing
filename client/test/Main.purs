module Test.Main where

import Prelude

import Effect (Effect)

import Test.Turing.Data.CardId as CardId
import Test.Turing.Data.Tape as Tape
import Test.Turing.Data.Execution as Execution
import Test.Turing.Utils.Random as Random

main :: Effect Unit
main = do
    CardId.main
    Tape.main
    Execution.main
    Random.main
