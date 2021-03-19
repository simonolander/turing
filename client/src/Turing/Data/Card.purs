module Turing.Data.Card where

import Prelude
import Turing.Data.Instruction

type Card =
    { id :: String
    , trueInstruction :: Instruction
    , falseInstruction :: Instruction
    }
