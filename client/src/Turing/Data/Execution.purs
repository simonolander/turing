module Turing.Data.Execution where

import Prelude
import Turing.Data.Program (Program)
import Turing.Data.Card (Card)
import Turing.Data.Tape (Tape, scan)
import Turing.Data.ExecutionState
import Data.Eq (class Eq)
import Data.Show (class Show)

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
