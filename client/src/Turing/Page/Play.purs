module Turing.Page.Play where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Turing.Component.Html.Utility (safeHref)
import Turing.Data.Campaign (Campaign, campaigns)
import Turing.Data.Route (Route(..))

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
            [ HH.h1_ [ HH.text "Play" ]
            , HH.h2_ [ HH.text "Campaigns" ]
            , HH.div_ $ renderCampaign <$> campaigns
            ]

    renderCampaign :: forall slots. Campaign -> HH.HTML (H.ComponentSlot slots m Action) Action
    renderCampaign campaign =
        HH.div_
            [ HH.h3_
                [ HH.a
                    [ safeHref $ Campaign campaign.id ]
                    [ HH.text campaign.name ]
                ]
            ]

    eval :: forall slots. H.HalogenQ query Action input ~> H.HalogenM State Action slots output m
    eval = H.mkEval H.defaultEval
