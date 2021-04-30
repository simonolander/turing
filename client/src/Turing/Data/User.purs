module Turing.Data.User where

import Prelude
import Data.Maybe (Maybe)

type User =
    { uid :: String
    , displayName :: Maybe String
    , isAnonymous :: Boolean
    }
