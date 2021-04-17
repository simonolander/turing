module Turing.Data.ExecutionState where

import Data.Eq (class Eq)
import Data.Show (class Show)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data ExecutionState
    = Writing
    | Moving
    | ChangingCard
    | Halted

derive instance eqExecutionState :: Eq ExecutionState
derive instance genericExecutionStore :: Generic ExecutionState _

instance showExecutionStore :: Show ExecutionState where
    show = genericShow
