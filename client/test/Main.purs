module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Test.Turing.Data.Machine as Machine

main :: Effect Unit
main = do
    log "🍝"
    log "You should add some tests."
    Machine.main
