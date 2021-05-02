module Turing.Page.SpecEditor where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Turing.AppM (AppM)
import Turing.Component.Form.Spec as SF
import Turing.Data.Spec (Spec, SpecId)
import Turing.Capability.ManageSpec (getSpec, saveSpec)
import Data.Const (Const)
import Formless as F
import Effect.Console (infoShow)
import Data.Maybe (Maybe(..))
import Network.RemoteData (RemoteData(..), fromEither)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Turing.Effect.Error (logError)

type State =
    { specId :: String
    , spec :: RemoteData Error (Maybe Spec)
    }

data Action
    = Initialize
    | HandleSpecForm Spec

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
    initialState specId = { specId, spec: NotAsked }

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
                            [ HH.text "Something went wrong when loading the spec. Check the logs for details." ]
                        ]
                Success (Nothing) ->
                    HH.div_
                        [ HH.h2_ [ HH.text "404 Not found" ]
                        , HH.p_ [ HH.text $ "Spec " <> state.specId <> " does not exists." ]
                        ]
                Success (Just spec) ->
                    HH.slot F._formless unit SF.component spec HandleSpecForm
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
            case result of
                Left error -> do
                    H.liftEffect $ logError error
                    H.modify_ _ { spec = Failure error }
                Right spec -> H.modify_ _ { spec = Success spec }
        handleAction (HandleSpecForm spec) = do
            result <- saveSpec spec
            H.liftEffect case result of
                Left error -> logError error
                Right error -> pure unit
