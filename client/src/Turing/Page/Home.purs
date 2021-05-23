module Turing.Page.Home where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Turing.Data.Route (Route(..))
import Turing.Component.Html.Utility (safeHref)

type State = Unit

type Action = Unit

type Slots :: forall k. Row k
type Slots = ()

type Input = Unit

type Output = Void

component :: forall query m. H.Component query Input Output m
component = H.mkComponent { initialState, render, eval }
    where
    initialState :: Input -> State
    initialState = const unit

    render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
    render _state =
        HH.div_
            [ HH.header_
                [ HH.h1_ [ HH.text "Turing" ] ]
            , HH.p_
                [ HH.a
                    [ safeHref Play ]
                    [ HH.text "Play" ]
                ]
            , HH.p_
                [ HH.a
                    [ safeHref Specs ]
                    [ HH.text "Specs" ]
                ]
            , HH.p_
                [ HH.a
                    [ safeHref Settings ]
                    [ HH.text "Settings" ]
                ]
            ]

    eval :: H.HalogenQ query Action Input ~> H.HalogenM State Action Slots Output m
    eval = H.mkEval H.defaultEval
