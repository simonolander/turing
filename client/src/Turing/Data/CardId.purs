module Turing.Data.CardId where

import Data.Eq (class Eq)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (class Ord)
import Data.Show (class Show)

newtype CardId = CardId String

derive instance eqCardId :: Eq CardId
derive instance ordCardId :: Ord CardId
derive instance newtypeCardId :: Newtype CardId _

instance showCardId :: Show CardId where
    show = unwrap
