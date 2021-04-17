module Turing.Data.Tape where

import Prelude

import Data.Map as Map
import Data.Maybe (fromMaybe)

type Tape = Map.Map Int Boolean

scan :: Int -> Tape -> Boolean
scan position tape =
    Map.lookup position tape # fromMaybe false

write :: Int -> Boolean -> Tape -> Tape
write = Map.insert

empty :: Tape
empty = Map.empty
