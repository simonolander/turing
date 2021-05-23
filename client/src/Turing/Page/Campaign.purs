module Turing.Page.Campaign where

import Prelude

import Data.Array (find)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..))
import Turing.Component.Html.Utility (safeHref)
import Turing.Data.Campaign (Campaign, CampaignId, campaigns)
import Turing.Data.Route (Route(..))
import Turing.Data.Spec (Spec)

type State =
    { campaignId :: CampaignId
    , campaign :: RemoteData String (Maybe Campaign)
    }

type Action = Unit

type Input = CampaignId

component :: forall query output m. H.Component query Input output m
component = H.mkComponent { initialState, render, eval }
    where
    initialState :: Input -> State
    initialState campaignId =
        { campaignId
        , campaign: campaigns
            # find (\ campaign -> campaign.id == campaignId)
            # Success
        }

    render :: forall slots. State -> HH.HTML (H.ComponentSlot slots m Action) Action
    render state =
        case state.campaign of
            NotAsked -> HH.text "Not asked"
            Loading ->
                HH.div_
                    [ HH.h1_ [ HH.text "Campaign" ]
                    , HH.p_ [ HH.text "Loadign campaign" ]
                    ]
            Failure error ->
                HH.div_
                    [ HH.h1_ [ HH.text "Campaign" ]
                    , HH.p_ [ HH.text error ]
                    ]
            Success Nothing ->
                HH.div_
                    [ HH.h1_ [ HH.text "Campaign - 404" ]
                    , HH.p_ [ HH.text $ "Campaign " <> state.campaignId <> " not found" ]
                    ]
            Success (Just campaign) ->
                HH.div_
                    [ HH.h1_ [ HH.text campaign.name ]
                    , HH.div_ $ renderSpec <$> campaign.specs
                    ]
            where
            renderSpec :: forall slots. Spec -> HH.HTML (H.ComponentSlot slots m Action) Action
            renderSpec spec =
                HH.h2_
                    [ HH.a
                        [ safeHref $ Spec spec.id ]
                        [ HH.text spec.name ]
                    ]

    eval :: forall slots. H.HalogenQ query Action Input ~> H.HalogenM State Action slots output m
    eval = H.mkEval H.defaultEval
