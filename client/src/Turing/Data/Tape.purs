module Turing.Data.Tape where

import Prelude
import Data.Map as Map

type Tape = Map.Map Int Boolean

empty :: Tape
empty = Map.empty
