module Turing.Component.Router where

import Prelude
import Data.Maybe (Maybe)
import Turing.Data.Route (Route(..))

type State =
    { route :: Maybe Route
    }

data Query a = Navigate Route a
