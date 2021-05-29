module Turing.Data.Env where

import Prelude
import Network.RemoteData (RemoteData)
import Data.Maybe (Maybe)
import Turing.Data.User (User)
import Foreign (MultipleErrors)
import Effect.Ref (Ref)

type Env
  = { logLevel :: LogLevel
    , userRef :: Ref (RemoteData MultipleErrors (Maybe User))
    }

data LogLevel
  = Dev
  | Prod
