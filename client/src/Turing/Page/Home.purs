module Turing.Page.Home where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Turing.Data.Route (Route(..))
import Turing.Component.Html.Utility (safeHref)
import Data.Newtype (wrap)

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
        HH.div
            [ HP.class_ $ wrap "container" ]
            [ HH.section
                [ HP.class_ $ wrap "header" ]
                [ HH.h1
                    [ HP.class_ $ wrap "title" ]
                    [ HH.text "Turing" ]
                ]
            , HH.div
                [ HP.class_ $ wrap "row" ]
                [ HH.a
                    [ safeHref Play
                    , HP.classes $ wrap <$> [ "button", "button-primary", "row" ]
                    ]
                    [ HH.text "Play" ]
                ]
            , HH.a
                [ safeHref Specs
                , HP.class_ $ wrap "button"
                ]
                [ HH.text "Specs" ]
            , HH.a
                [ safeHref Settings
                , HP.class_ $ wrap "button"
                ]
                [ HH.text "Settings" ]
            ]

    eval :: H.HalogenQ query Action Input ~> H.HalogenM State Action Slots Output m
    eval = H.mkEval H.defaultEval
