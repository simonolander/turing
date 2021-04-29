module Turing.Page.Template where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Turing.AppM (AppM)
import Data.Const (Const)

type State = Unit

type Action = Unit

type Slots :: forall k. Row k
type Slots = ()

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
    render = const $ HH.h1_ [ HH.text "Template" ]

    eval :: H.HalogenQ Query Action Input ~> H.HalogenM State Action Slots Output AppM
    eval = H.mkEval H.defaultEval
