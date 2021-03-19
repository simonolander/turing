module Turing.Data.Machine where

import Prelude
import Turing.Data.Tape as Tape
import Turing.Data.Card as Card
import Data.Map as Map
import Data.Maybe

data SubState
    = Read
    | Write
    | Move
    | ChangeCard

derive instance eqSubState :: Eq SubState

instance showSubState :: Show SubState where
    show Read = "Read"
    show Write = "Write"
    show Move = "Move"
    show ChangeCard = "ChangeCard"

type Machine =
    { tape :: Tape.Tape
    , position :: Int
    , cards :: Map.Map Int Card.Card
    , card :: Card.Card
    , subState :: SubState
    }

empty :: Machine
empty =
    { tape : Tape.empty
    , position : 0
    , cards : Map.empty
    , card : Card.empty
    , subState : Read
    }

getSubState :: Machine -> SubState
getSubState = _.subState

advanceSubState :: Machine -> Machine
advanceSubState machine =
    case machine.subState of
        Read -> machine { subState = Write }
        Write ->
            let
                { tape, position, card } = machine
                input = Map.lookup position tape
                    # fromMaybe false
                instruction =
                    if input then
                        card.trueInstruction
                    else
                        card.falseInstruction
                output = instruction.output
            in
                machine
                    { tape = Map.insert position output tape
                    , subState = Move
                    }
        Move -> machine
        ChangeCard -> machine
