module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Test.Turing.Data.CardId as CardId

main :: Effect Unit
main = do
    CardId.main
