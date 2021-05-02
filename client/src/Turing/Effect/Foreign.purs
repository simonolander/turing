module Turing.Effect.Foreign where

import Prelude
import Foreign (Foreign, isNull, FT)
import Data.Maybe (Maybe(..))

readNullable :: forall m a. Monad m => (Foreign -> FT m a) -> Foreign -> FT m (Maybe a)
readNullable read foreignValue =
    if isNull foreignValue then
        pure Nothing
    else do
        value <- read foreignValue
        pure $ Just value

