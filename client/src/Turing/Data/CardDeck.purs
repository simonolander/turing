module Turing.Data.CardDeck where

import Prelude
import Turing.Data.Card (Card)
import Turing.Data.CardId (CardId)
import Data.Map (Map, empty) as Map

type CardDeck = Map.Map CardId Card

empty :: CardDeck
empty = Map.empty
