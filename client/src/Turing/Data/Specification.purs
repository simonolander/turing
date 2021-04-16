--| A specification represents a single puzzle in this game
module Turing.Data.Specification where

import Prelude
import Turing.Data.Tape

type Specification =
    { initialTape :: Tape
    , maximumNumberOfCards :: Int
    }
