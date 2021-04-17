module Turing.Data.Instruction where

import Turing.Data.CardId (CardId)
import Data.Maybe (Maybe(..))

type Instruction =
    { writeSymbol :: Boolean
    , moveTapeLeft :: Boolean
    , nextCardId :: Maybe CardId
    }

empty :: Instruction
empty =
    { writeSymbol: false
    , moveTapeLeft: false
    , nextCardId: Nothing
    }
