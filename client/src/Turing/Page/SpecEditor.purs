module Turing.Page.SpecEditor where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Turing.AppM (AppM)
import Turing.Component.Form.Spec as SF
import Turing.Data.Spec (Spec, SpecId)
import Data.Const (Const)
import Formless as F
import Effect.Console (infoShow)
import Data.Maybe (Maybe(..))
import Network.RemoteData (RemoteData(..))

type State =
    { specId :: String
    , spec :: RemoteData Unit Spec
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
            [ HH.h1_ [ HH.text "Edit spec" ]
            , HH.slot F._formless unit SF.component state.specId HandleSpecForm
            ]

    eval :: H.HalogenQ Query Action Input ~> H.HalogenM State Action Slots Output AppM
    eval = H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
        where
        handleAction :: Action -> H.HalogenM State Action Slots Output AppM Unit
        handleAction Initialize = pure unit
        handleAction (HandleSpecForm spec) = H.liftEffect $ infoShow spec
