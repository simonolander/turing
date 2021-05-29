module Turing.Data.Card where

import Prelude
import Data.Tuple (Tuple)
import Data.Maybe (Maybe)

type CardId = String

type Instruction =
    { writeSymbol :: Boolean
    , tapeMotion :: Boolean
    , nextCardId :: Maybe CardId
    }

type Card =
    { id :: CardId
    , instructions :: Tuple Instruction Instruction
    }
