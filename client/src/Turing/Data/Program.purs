module Turing.Data.Program where

import Prelude

import Data.Map (Map)
import Turing.Data.Card (Card, CardId)

type ProgramId = String

type Program =
    { id :: ProgramId
    , deck :: Map CardId Card
    }
