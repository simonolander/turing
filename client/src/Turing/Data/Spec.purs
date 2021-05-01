module Turing.Data.Spec where

import Prelude
import Data.Map (Map)
import Turing.Effect.Random (randomString)
import Effect (Effect)

type SpecId = String

type Spec =
    { id :: SpecId
    , name :: String
    , maxNumberOfCards :: Int
--    , initialTape :: Map Int Boolean
--    , goal :: SpecGoal
--    , isTapeModifiable :: Boolean
    }

mkSpec :: Effect Spec
mkSpec = do
    id <- randomString 7
    pure { id, name: "New spec", maxNumberOfCards: 5 }

--data SpecGoal
--    = Maximize
--    | Specific SpecificGoal
--
--type SpecificGoal = Map Int Boolean
