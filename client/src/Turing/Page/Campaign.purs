module Turing.Page.Campaign where

import Prelude
import Data.Array (find)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Turing.Component.Html.Utility (safeHref)
import Turing.Data.Campaign (Campaign, CampaignId, campaigns)
import Turing.Data.Route (Route(..))
import Turing.Data.Spec (Spec)
import Turing.Component.Html.Utility (navbar)
import Turing.Component.Html.Utility (remoteData)
import Turing.Component.Html.Utility (section_)
import Turing.Component.Html.Utility (title_)
import Data.Newtype (wrap)

type State
  = { campaignId :: CampaignId
    , campaign :: RemoteData String (Maybe Campaign)
    }

type Action
  = Unit

type Input
  = CampaignId

component :: forall query output m. H.Component query Input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: Input -> State
  initialState campaignId =
    { campaignId
    , campaign:
        campaigns
          # find (\campaign -> campaign.id == campaignId)
          # Success
    }

  render :: forall slots. State -> HH.HTML (H.ComponentSlot slots m Action) Action
  render state =
    HH.div_
      [ navbar
      , remoteData state.campaign "campaign" state.campaignId renderCampaign
      ]
    where
    renderCampaign campaign =
      section_
        [ title_ campaign.name
        , HH.div
            [ HP.classes $ wrap <$> [ "columns", "is-multiline" ] ]
            $ renderSpec
            <$> campaign.specs
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

  eval :: forall slots. H.HalogenQ query Action Input ~> H.HalogenM State Action slots output m
  eval = H.mkEval H.defaultEval
