module Turing.Page.Home where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Turing.Data.Route (Route(..))
import Turing.Component.Html.Utility (safeHref)
import Data.Newtype (wrap)
import Turing.Component.Html.Utility (navbar)
import Turing.Component.Html.Utility (section_)
import Turing.Component.Html.Utility (container_)
import Turing.Component.Html.Utility (title_)

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
            [ navbar
            , HH.section
                [ HP.class_ $ wrap "hero is-medium" ]
                [ HH.div
                    [ HP.class_ $ wrap "hero-body" ]
                    [ HH.h1
                        [ HP.classes $ wrap <$> [ "title", "is-1", "has-text-centered" ] ]
                        [ HH.text "Turing" ]
                    , HH.h2
                        [ HP.classes $ wrap <$> [ "subtitle", "is-4", "has-text-centered" ] ]
                        [ HH.text "Fun with finite-state machines" ]
                    ]
                ]
            , container_
                [ HH.div
                    [ HP.classes $ wrap <$> [ "buttons", "are-large" ] ]
                    [ HH.a
                        [ safeHref Play
                        , HP.classes $ wrap <$> [ "button", "is-primary", "is-fullwidth" ]
                        ]
                        [ HH.text "Play" ]
                    , HH.a
                        [ safeHref Specs
                        , HP.classes $ wrap <$> [ "button", "is-fullwidth" ]
                        ]
                        [ HH.text "Specs" ]
                    , HH.a
                        [ safeHref Settings
                        , HP.classes $ wrap <$> [ "button", "is-fullwidth" ]
                        ]
                        [ HH.text "Settings" ]
                    ]
                ]
            ]

    eval :: H.HalogenQ query Action Input ~> H.HalogenM State Action Slots Output m
    eval = H.mkEval H.defaultEval
