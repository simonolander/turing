module Turing.Data.Machine where

import Prelude
import Turing.Data.Tape
import Turing.Data.Card
import Data.Map

type Machine =
    { tape :: Tape
    , position :: Int
    , cards :: Map Int Card
    , card :: Card
    }
