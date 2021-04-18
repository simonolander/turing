--| A spec represents a single puzzle in this game
module Turing.Data.Spec where

import Prelude
import Turing.Data.Tape as Tape
import Data.Eq (class Eq)
import Data.Ord (class Ord)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show (class Show)
import Data.Maybe

newtype SpecId = SpecId String

derive instance eqSpecId :: Eq SpecId
derive instance ordSpecId :: Ord SpecId
derive instance newtypeSpecId :: Newtype SpecId _
instance showSpecId :: Show SpecId where
    show = unwrap

parseId :: String -> Maybe SpecId
parseId "" = Nothing
parseId str = Just $ wrap str

type Spec =
    { id :: SpecId
    , initialTape :: Tape.Tape
    , maximumNumberOfCards :: Int
    }

createSpec :: SpecId -> Spec
createSpec id =
    { id
    , initialTape: Tape.empty
    , maximumNumberOfCards: 0
    }
