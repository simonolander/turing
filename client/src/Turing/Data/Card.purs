module Turing.Data.Card where

import Prelude
import Data.Tuple
import Turing.Data.Instruction
import Turing.Data.CardId

type Card =
    { id :: CardId
    , instructions :: Tuple Instruction Instruction
    }
