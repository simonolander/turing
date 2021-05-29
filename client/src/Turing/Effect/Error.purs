module Turing.Effect.Error where

import Prelude
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (error) as C
import Effect.Exception (Error, name, message)
import Data.Either

logError :: Error -> Effect Unit
logError error = C.error $ showError error

showError :: Error -> String
showError error = name error <> ": " <> message error

hushError :: forall a b. Show a => Either a b -> Effect (Maybe b)
hushError (Left e) = do
  C.error $ show e
  pure Nothing

hushError (Right v) = pure $ Just v
