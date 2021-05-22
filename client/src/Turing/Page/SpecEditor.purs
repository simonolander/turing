module Turing.Page.SpecEditor where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..), fromEither)
import Turing.AppM (AppM)
import Turing.Capability.ManageSpec (getSpec, saveSpec)
import Turing.Component.Form.Spec as SF
import Turing.Data.Spec (Spec, SpecId)

type State =
    { specId :: String
    , spec :: RemoteData String (Maybe Spec)
    , savingSpec :: RemoteData String Unit
    }

data Action
    = Initialize
    | HandleSpecForm Spec
    | ClickedTestRun

type Slots =
    ( formless :: SF.Slot Unit )

type Query :: forall k. k -> Type
type Query = Const Void

type Input = SpecId

type Output = Void

component :: H.Component Query Input Output AppM
component = H.mkComponent { initialState, render, eval }
    where
    initialState :: Input -> State
    initialState specId = { specId, spec: NotAsked, savingSpec: NotAsked }

    render :: State -> HH.HTML (H.ComponentSlot Slots AppM Action) Action
    render state =
        HH.div_
            [ HH.h1_ [ HH.text $ "Spec " <> state.specId ]
            , case state.spec of
                NotAsked -> HH.text "Not asked"
                Loading -> HH.text "Loading spec"
                Failure error ->
                    HH.div_
                        [ HH.h2_ [ HH.text "Error loading spec" ]
                        , HH.p_
                            [ HH.text "Something went wrong when loading the spec." ]
                        , HH.p_
                            [ HH.text error ]
                        ]
                Success (Nothing) ->
                    HH.div_
                        [ HH.h2_ [ HH.text "404 Not found" ]
                        , HH.p_ [ HH.text $ "Spec " <> state.specId <> " does not exists." ]
                        ]
                Success (Just spec) ->
                    HH.slot F._formless unit SF.component spec HandleSpecForm
            , case state.savingSpec of
                NotAsked -> HH.text ""
                Loading -> HH.p_ [ HH.text "Saving spec" ]
                Failure error -> HH.p_ [ HH.text error ]
                Success _ -> HH.p_ [ HH.text "Spec saved 👌" ]
            , HH.button_ [ HH.text "Test run" ]
            ]

    eval :: H.HalogenQ Query Action Input ~> H.HalogenM State Action Slots Output AppM
    eval = H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
        where
        handleAction :: Action -> H.HalogenM State Action Slots Output AppM Unit
        handleAction Initialize = do
            specId <- H.gets _.specId
            H.modify_ _ { spec = Loading }
            result <- getSpec specId
            H.modify_ _ { spec = fromEither result}
        handleAction (HandleSpecForm spec) = do
            H.modify_ _ { savingSpec = Loading }
            result <- saveSpec spec
            H.modify_ _ { savingSpec = fromEither result }
        handleAction ClickedTestRun = pure unit
