module Turing.Data.Env where

import Prelude
import Network.RemoteData (RemoteData)
import Data.Maybe (Maybe)
import Turing.Data.User (User)
import Foreign (MultipleErrors)

type Env =
    { logLevel :: LogLevel
    , user :: RemoteData MultipleErrors (Maybe User)
    }

data LogLevel
    = Dev
    | Prod

