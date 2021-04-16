module Turing.Data.Instruction where

import Prelude

type Instruction =
    { writeSymbol :: Boolean
    , moveTape :: Boolean
    , nextCard :: String
    }
