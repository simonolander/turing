module Turing.Data.Env where

import Prelude
import Network.RemoteData (RemoteData)
import Data.Maybe (Maybe)

type Env =
    { logLevel :: LogLevel
--    , user :: RemoteData Void (Maybe User)
    }

data LogLevel
    = Dev
    | Prod

