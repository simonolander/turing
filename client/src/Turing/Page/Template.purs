module Turing.Page.Template where

import Prelude
import Halogen as H
import Halogen.HTML as HH

type State = Unit

type Action = Unit

type Slots :: forall k. Row k
type Slots = ()

type Input = Unit

type Output = Unit

component :: forall query m. H.Component query Input Output m
component = H.mkComponent { initialState, render, eval }
    where
    initialState :: Input -> State
    initialState = const unit

    render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
    render = const $ HH.text "My component"

    eval :: H.HalogenQ query Action Input ~> H.HalogenM State Action Slots Output m
    eval = H.mkEval H.defaultEval