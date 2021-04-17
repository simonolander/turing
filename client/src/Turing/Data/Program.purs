module Turing.Data.Program where

import Turing.Data.CardDeck (CardDeck, empty)
import Turing.Data.Card (Card)

type Program =
    { deck :: CardDeck
    , initialCard :: Card
    , initialPosition :: Int
    }

program :: Card -> Program
program initialCard =
    { deck: empty
    , initialCard
    , initialPosition: 0
    }
