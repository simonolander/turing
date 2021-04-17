module Turing.Data.Execution where

import Prelude
import Turing.Data.Program (Program)
import Turing.Data.Card (Card)
import Turing.Data.Tape (Tape, scan)
import Data.Eq (class Eq)

data ExecutionState
    = Scanning
    | Writing Boolean
    | Moving
    | Halted

derive instance eqExecutionState :: Eq ExecutionState

type Execution =
    { program :: Program
    , tape :: Tape
    , position :: Int
    , card :: Card
    , state :: ExecutionState
    }

subStep :: Execution -> Execution
subStep execution =
    case execution.state of
        Scanning ->
            let
                tapeSymbol = scan execution.position execution.tape
            in
            execution
                { state = Writing tapeSymbol }

        _ -> execution
