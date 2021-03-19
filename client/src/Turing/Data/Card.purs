module Turing.Data.Card where

import Prelude
import Turing.Data.Instruction as Instruction

type Card =
    { id :: String
    , trueInstruction :: Instruction.Instruction
    , falseInstruction :: Instruction.Instruction
    }

empty :: Card
empty =
    { id : ""
    , trueInstruction : Instruction.empty
    , falseInstruction : Instruction.empty
    }
