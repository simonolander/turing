module Turing.Component.Router where

import Prelude
import Data.Maybe
import Turing.Data.Route
import Turing.Component.Utils
import Halogen as H
import Turing.Capability.Navigate
import Effect.Aff.Class (class MonadAff)
import Halogen.HTML as HH

type State =
    { route :: Maybe Route }

data Query a = Navigate Route a

data Action = Initialize

type Input = {}

type Output = Void

type ChildSlots =
    ( home :: OpaqueSlot Unit )

component :: forall m. MonadAff m => Navigate m => H.Component Query Input Output m
component = H.mkComponent { initialState, render, eval }
    where
        initialState :: Input -> State
        initialState _ = { route: Nothing }

        render state = HH.text "hej"

        eval = H.mkEval H.defaultEval
