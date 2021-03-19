module Turing.Data.Instruction where

import Prelude
import Data.Maybe

type Instruction =
    { write :: Boolean
    , move :: Boolean
    , nextCard :: Maybe String
    }
