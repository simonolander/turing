module Turing.Data.CardDeck where

import Prelude
import Turing.Data.Card (Card)
import Turing.Data.CardId (CardId)
import Data.Map (Map)

type CardDeck = Map CardId Card

