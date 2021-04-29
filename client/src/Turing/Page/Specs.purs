module Turing.Page.Specs where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Formless as F
import Data.Const (Const)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Turing.Component.Form.Spec as SF

type State = Unit

data Action = HandleSpecForm SF.Contact

type Slots =
    ( formless :: SF.Slot Unit )

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
        HH.h1_
            [ HH.text "Specs"
            , HH.slot F._formless unit SF.component unit HandleSpecForm
            ]

    eval :: H.HalogenQ Query Action Input ~> H.HalogenM State Action Slots Output m
    eval = H.mkEval H.defaultEval

type Spec =
    { name :: String
    , maxNumberOfCards :: Int
    }
