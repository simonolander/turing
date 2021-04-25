module Turing.Data.Env where

import Prelude

type Env =
    { logLevel :: LogLevel }

data LogLevel
    = Dev
    | Prod

