module Turing.Page.Play where

import Prelude

import Data.Newtype (wrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Turing.Component.Html.Utility (container_, navbar, safeHref, section_, title_)
import Turing.Data.Campaign (Campaign, campaigns)
import Turing.Data.Route (Route(..))
import Turing.Component.Html.Utility (title2_)

type State = Unit

type Action = Unit

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
                [ title_ "Campaigns"
                , HH.div
                    [ HP.class_ $ wrap "buttons" ]
                    $ renderCampaign <$> campaigns
                ]
            ]

    renderCampaign :: forall slots. Campaign -> HH.HTML (H.ComponentSlot slots m Action) Action
    renderCampaign campaign =
        HH.a
            [ safeHref $ Campaign campaign.id
            , HP.classes $ wrap <$> [ "button", "is-info" ]
            ]
            [ HH.text campaign.name ]

    eval :: forall slots. H.HalogenQ query Action input ~> H.HalogenM State Action slots output m
    eval = H.mkEval H.defaultEval
