module Turing.Data.Program where

import Prelude
import Turing.Data.CardDeck (CardDeck)
import Turing.Data.Card (Card)

type Program =
    { deck :: CardDeck
    , initialCard :: Card
    , initialPosition :: Int
    }
