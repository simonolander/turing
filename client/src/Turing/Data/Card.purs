module Turing.Data.Card where

import Prelude
import Data.Tuple
import Turing.Data.Instruction as Inst
import Turing.Data.CardId (CardId(..))

type Card =
    { id :: CardId
    , instructions :: Tuple Inst.Instruction Inst.Instruction
    }

empty :: Card
empty =
    { id: CardId ""
    , instructions: Tuple Inst.empty Inst.empty
    }

instruction :: Boolean -> Card -> Inst.Instruction
instruction true card = fst card.instructions
instruction false card = snd card.instructions
