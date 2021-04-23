module Turing.Capability.Firebase where

import Prelude
import Effect (Effect)
import Control.Promise (Promise)

foreign import data Auth :: Type
foreign import auth :: Effect Auth

foreign import signInAnonymously :: Auth -> Effect (Promise String)
