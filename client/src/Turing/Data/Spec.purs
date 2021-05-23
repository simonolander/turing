module Turing.Data.Spec where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Effect (Effect)
import Turing.Effect.Random (randomString)

type SpecId = String

type Spec =
    { id :: SpecId
    , name :: String
    , maxNumberOfCards :: Int
    , goal :: SpecGoal
--    , initialTape :: Map Int Boolean
--    , goal :: SpecGoal
--    , isTapeModifiable :: Boolean
    }

--| Returns a new spec with a random id
mkSpec :: Effect Spec
mkSpec = do
    id <- randomString 7
    pure
        { id
        , name: "New spec"
        , maxNumberOfCards: 5
        , goal: BusyBeaver
        }

data SpecGoal
    = BusyBeaver

derive instance genericSpecGoal :: Generic SpecGoal _

instance encodeJsonSpecGoal :: EncodeJson SpecGoal where
  encodeJson = genericEncodeJson

instance decodeJsonSpecGoal :: DecodeJson SpecGoal where
  decodeJson = genericDecodeJson
