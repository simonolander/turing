module Turing.Page.SpecEditor where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Turing.AppM (AppM)
import Turing.Component.Form.Spec as SF
import Turing.Data.Spec (Spec)
import Data.Const (Const)
import Formless as F
import Effect.Console (infoShow)

type State = Unit

data Action = HandleSpecForm Spec

type Slots =
    ( formless :: SF.Slot Unit )

type Query :: forall k. k -> Type
type Query = Const Unit

type Input = Unit

type Output = Void

component :: H.Component Query Input Output AppM
component = H.mkComponent { initialState, render, eval }
    where
    initialState :: Input -> State
    initialState = const unit

    render :: State -> HH.HTML (H.ComponentSlot Slots AppM Action) Action
    render _state =
        HH.div_
            [ HH.h1_ [ HH.text "Edit spec" ]
            , HH.slot F._formless unit SF.component "unit" HandleSpecForm
            ]

    eval :: H.HalogenQ Query Action Input ~> H.HalogenM State Action Slots Output AppM
    eval = H.mkEval H.defaultEval { handleAction = handleAction }
        where
        handleAction :: Action -> H.HalogenM State Action Slots Output AppM Unit
        handleAction (HandleSpecForm spec) = H.liftEffect $ infoShow spec
