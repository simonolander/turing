--| A specification represents a single puzzle in this game
module Turing.Data.Specification where

import Prelude
import Turing.Data.Tape as Tape
import Data.Eq (class Eq)
import Data.Ord (class Ord)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show (class Show)
import Data.Maybe

newtype SpecificationId = SpecificationId String

derive instance eqSpecificationId :: Eq SpecificationId
derive instance ordSpecificationId :: Ord SpecificationId
derive instance newtypeSpecificationId :: Newtype SpecificationId _
instance showSpecificationId :: Show SpecificationId where
    show = unwrap

parseId :: String -> Maybe SpecificationId
parseId "" = Nothing
parseId str = Just $ wrap str

type Specification =
    { id :: SpecificationId
    , initialTape :: Tape.Tape
    , maximumNumberOfCards :: Int
    }

createSpecification :: SpecificationId -> Specification
createSpecification id =
    { id
    , initialTape: Tape.empty
    , maximumNumberOfCards: 0
    }
