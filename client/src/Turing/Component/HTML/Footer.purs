-- | This module exports a pure HTML function to render a consistent footer throughout the app.
module Turing.Component.HTML.Footer where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Turing.Component.HTML.Utils (css)

footer :: forall i p. HH.HTML i p
footer =
  HH.footer_
    [ HH.div
      [ css "container" ]
      [ HH.a
        [ css "logo-font"
        , HP.href "/"
        ]
        [ HH.text "Turing" ]
      , HH.span
        [ css "attribution" ]
        [ HH.text "An interactive learning project from "
        , HH.a
          [ HP.href "https://thinkster.io" ]
          [ HH.text "Thinkster" ]
        , HH.text ". Code & design licensed under MIT. Implemented by "
        , HH.a
          [ HP.href "https://thomashoneyman.com" ]
          [ HH.text "Thomas Honeyman" ]
        , HH.text "."
        ]
      ]
    ]
