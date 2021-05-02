module Turing.Effect.Error where

import Prelude
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Exception as Ex
import Effect.Console as C
import Data.Maybe (fromMaybe)

logError :: Error -> Effect Unit
logError error = do
    C.error $ Ex.name error
        <> ": "
        <> Ex.message error
--        <> "\n"
--        <> fromMaybe "<no stacktrace>" (Ex.stack error)
