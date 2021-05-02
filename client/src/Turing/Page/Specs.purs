module Turing.Page.Specs where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Const (Const)
import Turing.Data.Spec (mkSpec)
import Turing.AppM (AppM)

import Debug (traceM)

type State = Unit

data Action =
    ClickedNewSpec

type Slots :: forall k. Row k
type Slots = ()

type Query :: forall k. k -> Type
type Query = Const Void

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
            [ HH.h1_ [ HH.text "Specs" ]
            , HH.button
                [ HE.onClick $ const ClickedNewSpec ]
                [ HH.text "New spec" ]
            ]

    eval :: H.HalogenQ Query Action Input ~> H.HalogenM State Action Slots Output AppM
    eval = H.mkEval $ H.defaultEval { handleAction = handleAction }
        where
        handleAction :: Action -> H.HalogenM State Action Slots Output AppM Unit
        handleAction ClickedNewSpec = H.liftEffect do
            spec <- mkSpec
            traceM spec
