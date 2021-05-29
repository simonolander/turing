module Turing.Page.Play where

import Prelude
import Data.Newtype (wrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Turing.Component.Html.Utility (container_, navbar, safeHref, section_, title2_, title_)
import Turing.Data.Campaign (Campaign, campaigns)
import Turing.Data.Route (Route(..))
import Turing.Data.Spec (Spec)

type State
  = Unit

type Action
  = Unit

component :: forall query input output m. H.Component query input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: input -> State
  initialState = const unit

  render :: forall slots. State -> HH.HTML (H.ComponentSlot slots m Action) Action
  render _ =
    HH.div_
      [ navbar
      , section_
          [ HH.h1
              [ HP.classes $ wrap <$> [ "title", "has-text-centered", "is-1" ] ]
              [ HH.text "Campaigns" ]
          ]
      , campaigns <#> renderCampaign # HH.div_
      ]

  renderCampaign :: forall slots. Campaign -> HH.HTML (H.ComponentSlot slots m Action) Action
  renderCampaign campaign =
    HH.div_
      [ HH.div
          [ HP.classes $ wrap <$> [ "hero", "is-primary" ] ]
          [ HH.div
              [ HP.classes $ wrap <$> [ "hero-body" ] ]
              [ HH.h1
                  [ HP.classes $ wrap <$> [ "title" ] ]
                  [ HH.text campaign.name ]
              ]
          ]
      , section_
          [ container_
              [ HH.div
                  [ HP.classes $ wrap <$> [ "columns", "is-multiline" ] ]
                  $ renderSpec
                  <$> campaign.specs
              ]
          ]
      ]

  renderSpec :: forall slots. Spec -> HH.HTML (H.ComponentSlot slots m Action) Action
  renderSpec spec =
    HH.div
      [ HP.classes $ wrap <$> [ "column", "is-narrow" ] ]
      [ HH.div
          [ HP.class_ $ wrap "card" ]
          [ HH.div
              [ HP.class_ $ wrap "card-content" ]
              [ HH.h1
                  [ HP.classes $ wrap <$> [ "title", "is-4" ] ]
                  [ HH.text spec.name ]
              ]
          , HH.footer
              [ HP.class_ $ wrap "card-footer" ]
              [ HH.a
                  [ HP.class_ $ wrap "card-footer-item"
                  , safeHref $ Spec spec.id
                  ]
                  [ HH.text "Play" ]
              ]
          ]
      ]

  eval :: forall slots. H.HalogenQ query Action input ~> H.HalogenM State Action slots output m
  eval = H.mkEval H.defaultEval
