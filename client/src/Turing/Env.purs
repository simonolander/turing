module Turing.Env where

import Prelude

--| Determines what messages to log from the application
data LogLevel
    = Debug
    | Info

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel


type Env =
    { logLevel :: LogLevel
    , baseUrl :: String
    }
