module Turing.Page.Specs where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Const (Const)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Turing.Component.Form.Spec as SF
import Effect.Console as Console

type State = Unit

data Action =
    ClickedNewSpec

type Slots :: forall k. Row k
type Slots = ()

type Query :: forall k. k -> Type
type Query = Const Void

type Input = Unit

type Output = Void

component :: forall m.
    MonadEffect m =>
    MonadAff m =>
    H.Component Query Input Output m
component = H.mkComponent { initialState, render, eval }
    where
    initialState :: Input -> State
    initialState = const unit

    render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
    render _state =
        HH.div_
            [ HH.h1_ [ HH.text "Specs" ]
            , HH.button
                [ HE.onClick $ const ClickedNewSpec ]
                [ HH.text "New spec" ]
            ]

    eval :: H.HalogenQ Query Action Input ~> H.HalogenM State Action Slots Output m
    eval = H.mkEval $ H.defaultEval { handleAction = handleAction }
        where
        handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
        handleAction ClickedNewSpec = do
            pure unit

