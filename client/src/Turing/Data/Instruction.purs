module Turing.Data.Instruction where

import Prelude
import Data.Maybe

type Instruction =
    { output :: Boolean
    , moveRight :: Boolean
    , nextCardId :: Maybe String
    }

empty :: Instruction
empty =
    { output : false
    , moveRight : false
    , nextCardId : Nothing
    }
