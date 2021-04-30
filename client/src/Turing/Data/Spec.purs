module Turing.Data.Spec where

import Prelude
import Data.Map (Map)

type SpecId = String

type Spec =
    { id :: SpecId
    , name :: String
    , maxNumberOfCards :: Int
--    , initialTape :: Map Int Boolean
--    , goal :: SpecGoal
--    , isTapeModifiable :: Boolean
    }

--data SpecGoal
--    = Maximize
--    | Specific SpecificGoal
--
--type SpecificGoal = Map Int Boolean
