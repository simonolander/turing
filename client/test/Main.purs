module Test.Main where

import Prelude

import Effect (Effect)

import Test.Turing.Data.CardId as CardId
import Test.Turing.Data.Tape as Tape

main :: Effect Unit
main = do
    CardId.main
    Tape.main
