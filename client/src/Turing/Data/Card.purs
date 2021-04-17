module Turing.Data.Card where

import Prelude
import Data.Tuple
import Turing.Data.Instruction (Instruction)
import Turing.Data.CardId (CardId)

type Card =
    { id :: CardId
    , instructions :: Tuple Instruction Instruction
    }
