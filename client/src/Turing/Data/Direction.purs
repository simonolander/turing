module Turing.Data.Direction where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Show (class Show)

data Direction
    = Left
    | Right

derive instance eqDirection :: Eq Direction
derive instance genericDirection :: Generic Direction _
instance showDirection :: Show Direction where
    show = genericShow

