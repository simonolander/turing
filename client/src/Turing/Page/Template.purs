module Turing.Page.Template where

import Prelude
import Halogen as H
import Halogen.HTML as HH

--import Halogen.HTML.Events as HE
type State
  = Unit

component :: forall query input output m. H.Component query input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: input -> State
  initialState = const unit

  render :: forall slots action. State -> HH.HTML (H.ComponentSlot slots m action) action
  render = const $ HH.h1_ [ HH.text "Template" ]

  eval :: forall slots action. H.HalogenQ query action input ~> H.HalogenM State action slots output m
  eval = H.mkEval H.defaultEval
