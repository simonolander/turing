module Turing.Data.Program where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Effect (Effect)
import Turing.Data.Card (Card, CardId)
import Turing.Data.Spec (Spec, SpecId)
import Turing.Effect.Random (randomId)

type ProgramId = String

type Program =
    { id :: ProgramId
    , name :: String
    , specId :: SpecId
    , deck :: Map CardId Card
    }

-- | Creates an empty program with a random id and the given spec id
mkProgram :: Spec -> Effect Program
mkProgram spec = do
    id <- randomId
    pure
        { id
        , name: "My program"
        , specId: spec.id
        , deck: Map.empty
        }
