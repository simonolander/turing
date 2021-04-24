module Turing.Capability.Firebase where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff)
import Control.Promise (Promise, toAff)
import Turing.Data.Firebase (UserCredential)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, JsonDecodeError)
import Data.Either (Either)

foreign import data Auth :: Type

foreign import auth :: Effect Auth

foreign import signInAnonymouslyImpl :: Auth -> Effect (Promise Json)

signInAnonymously :: Auth -> Aff (Either JsonDecodeError UserCredential)
signInAnonymously auth' = do
    promise <- liftEffect $ signInAnonymouslyImpl auth'
    json <- toAff promise
    liftEffect $ pure $ decodeJson json
