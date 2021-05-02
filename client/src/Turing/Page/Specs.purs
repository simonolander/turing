module Turing.Page.Specs where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Const (Const)
import Data.Either (Either(..))
import Turing.Data.Spec (mkSpec)
import Turing.Data.Route (Route(..))
import Turing.AppM (AppM)
import Turing.Capability.ManageSpec (saveSpec)
import Turing.Capability.Navigate (navigate)
import Network.RemoteData (RemoteData(..), isLoading)
import Effect.Exception (Error)
import Turing.Effect.Error (logError)

type State =
    { newSpecCreation :: RemoteData Error Void
    }

data Action =
    ClickedNewSpec

type Slots :: forall k. Row k
type Slots = ()

type Query :: forall k. k -> Type
type Query = Const Void

type Input = Unit

type Output = Void

component :: H.Component Query Input Output AppM
component = H.mkComponent { initialState, render, eval }
    where
    initialState :: Input -> State
    initialState _input = { newSpecCreation: NotAsked }

    render :: State -> HH.HTML (H.ComponentSlot Slots AppM Action) Action
    render state =
        HH.div_
            [ HH.h1_ [ HH.text "Specs" ]
            , HH.button
                [ HE.onClick $ const ClickedNewSpec
                , HP.disabled $ isLoading state.newSpecCreation
                ]
                [ HH.text "New spec" ]
            , HH.text case state.newSpecCreation of
                Failure error -> "Error when creating spec, see logs for details."
                _ -> ""
            ]

    eval :: H.HalogenQ Query Action Input ~> H.HalogenM State Action Slots Output AppM
    eval = H.mkEval $ H.defaultEval { handleAction = handleAction }
        where
        handleAction :: Action -> H.HalogenM State Action Slots Output AppM Unit
        handleAction ClickedNewSpec = do
            H.modify_ _ { newSpecCreation = Loading }
            spec <- H.liftEffect mkSpec
            result <- saveSpec spec
            case result of
                Left error -> do
                    H.liftEffect $ logError error
                    H.modify_ _ { newSpecCreation = Failure error }
                Right _ -> navigate $ SpecEditor spec.id
