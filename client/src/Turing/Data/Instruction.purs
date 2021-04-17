module Turing.Data.Instruction where

import Turing.Data.CardId (CardId)
import Turing.Data.Direction (Direction(..))
import Data.Maybe (Maybe(..))

type Instruction =
    { writeSymbol :: Boolean
    , moveDirection :: Direction
    , nextCardId :: Maybe CardId
    }

empty :: Instruction
empty =
    { writeSymbol: false
    , moveDirection : Right
    , nextCardId: Nothing
    }
