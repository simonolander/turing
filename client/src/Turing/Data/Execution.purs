module Turing.Data.Execution where

import Prelude
import Turing.Data.Program (Program)
import Turing.Data.Card as Card
import Turing.Data.Tape as Tape
import Turing.Data.ExecutionState
import Turing.Data.Specification
import Turing.Data.Direction
import Turing.Data.Instruction as Inst
import Data.Eq (class Eq)
import Data.Show (class Show)
import Data.Maybe
import Data.List
import Data.Map as Map

type Execution =
    { specification :: Specification
    , program :: Program
    , history :: List ExecutionSubStep
    }

type ExecutionSubStep =
    { tape :: Tape.Tape
    , position :: Int
    , card :: Card.Card
    , state :: ExecutionState
    }


subStep :: Execution -> Execution
subStep execution =
    execution
        { history = subStep' execution.history : execution.history }
    where

    getInstruction :: List ExecutionSubStep -> Inst.Instruction
    getInstruction Nil =
        let
            tape = execution.specification.initialTape
            position = execution.program.initialPosition
            scannedSymbol = Tape.scan position tape
            card = execution.program.initialCard
        in
        Card.instruction scannedSymbol card
    getInstruction (step@{tape, position, card, state} : t) =
        case state of
            Writing ->
                let
                    scannedSymbol = Tape.scan position tape
                in
                Card.instruction scannedSymbol card
            _ -> getInstruction t

    subStep' :: List ExecutionSubStep -> ExecutionSubStep
    subStep' Nil =
        { tape: Tape.empty
        , position: execution.program.initialPosition
        , card: execution.program.initialCard
        , state: Writing
        }
    subStep' list@(step@{tape, position, card, state} : t) =
        let
            instruction = getInstruction list
        in
        case state of
            Writing ->
                step
                    { state = Moving
                    , tape = Tape.write position instruction.writeSymbol tape
                    }
            Moving ->
                step
                    { state = ChangingCard
                    , position =
                        case instruction.moveDirection of
                            Left -> position - 1
                            Right -> position + 1
                    }
            ChangingCard ->
                case instruction.nextCardId of
                    Just nextCardId ->
                        case Map.lookup nextCardId execution.program.deck of
                            Just card ->
                                step
                                    { state = Writing
                                    , card = card
                                    }
                            Nothing ->
                                step { state = Halted }
                    Nothing -> step { state = Halted }
            Halted -> step

