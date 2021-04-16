module Turing.Data.Card where

import Prelude
import Data.Tuple
import Turing.Data.Instruction

type Card =
    { id :: String
    , instructions :: Tuple Instruction Instruction
    }
