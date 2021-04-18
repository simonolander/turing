--| A specification represents a single puzzle in this game
module Turing.Data.Specification where

import Prelude
import Turing.Data.Tape as Tape

type SpecificationId = String

type Specification =
    { id :: SpecificationId
    , initialTape :: Tape.Tape
    , maximumNumberOfCards :: Int
    }

createSpecification :: SpecificationId -> Specification
createSpecification id =
    { id
    , initialTape: Tape.empty
    , maximumNumberOfCards: 0
    }
