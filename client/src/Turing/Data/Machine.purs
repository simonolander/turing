module Turing.Data.Machine where

import Prelude
import Turing.Data.Tape as Tape
import Turing.Data.Card as Card
import Data.Map as Map
import Data.Maybe
import Turing.Optics

data SubState
    = Read
    | Write
    | Move Boolean
    | ChangeCard Boolean

derive instance eqSubState :: Eq SubState

instance showSubState :: Show SubState where
    show Read = "Read"
    show Write = "Write"
    show (Move a) = "Move " <> show a
    show (ChangeCard a) = "ChangeCard " <> show a

type Machine =
    { tape :: Tape.Tape
    , position :: Int
    , cards :: Map.Map String Card.Card
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

subStep :: Machine -> Machine
subStep machine =
    let
        { tape, position, card, cards, subState } = machine
    in
        case subState of
            Read -> set _subState Write machine
            Write ->
                let
                    input = view (at position) tape
                        # fromMaybe false
                    instruction =
                        if input then
                            card.trueInstruction
                        else
                            card.falseInstruction
                    output = instruction.output
                in
                    machine
                        # set _subState (Move input)
                        # set (_tape <<< at position) (Just output)
            _ -> machine
