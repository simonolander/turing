module Turing.Component.Router where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Data.Maybe (Maybe(..), fromMaybe)
import Turing.Capability.Navigate (navigate)
import Turing.AppM (AppM)
import Turing.Data.Route (Route(..), route)
import Turing.Page.Home as Home
import Turing.Page.Specs as Specs
import Turing.Page.SpecEditor as SpecEditor
import Turing.Page.Settings as Settings
import Type.Proxy (Proxy(..))
import Routing.Hash (getHash)
import Routing.Duplex (parse)
import Data.Either (hush)
import Data.Const (Const)

type State =
    { route :: Maybe Route
    }

data Query a = Navigate Route a

data Action
    = Initialize

type Slots =
    ( home :: H.Slot (Const Void) Void Unit
    , specs :: H.Slot (Const Void) Void Unit
    , specEditor :: H.Slot (Const Void) Void Unit
    , settings :: H.Slot (Const Void) Void Unit
    )

type Input = Unit

type Output = Void

component :: H.Component Query Input Output AppM
component = H.mkComponent { initialState, render, eval }
    where
    initialState :: Input -> State
    initialState = const { route: Nothing }

    render :: State -> HH.HTML (H.ComponentSlot Slots AppM Action) Action
    render state =
        case state.route of
            Just Home -> HH.slot (Proxy :: _ "home") unit Home.component unit absurd
            Just Specs -> HH.slot (Proxy :: _ "specs") unit Specs.component unit absurd
            Just Settings -> HH.slot (Proxy :: _ "settings") unit Settings.component unit absurd
            Just (SpecEditor _specId) -> HH.slot (Proxy :: _ "specEditor") unit SpecEditor.component unit absurd
            Nothing -> HH.text "404 Not found"

    eval :: H.HalogenQ Query Action Input ~> H.HalogenM State Action Slots Output AppM
    eval = H.mkEval H.defaultEval
        { initialize = initialize
        , handleAction = handleAction
        , handleQuery = handleQuery
        }
        where
        initialize :: Maybe Action
        initialize = Just Initialize

        handleAction :: Action -> H.HalogenM State Action Slots Output AppM Unit
        handleAction action = case action of
            Initialize -> do
                initialRoute <- H.liftEffect $ hush <<< (parse route) <$> getHash
                navigate $ fromMaybe Home initialRoute

        handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Output AppM (Maybe a)
        handleQuery query = case query of
            Navigate newRoute a -> do
                { route } <- H.get
                when (route /= Just newRoute) do
                    H.modify_ _ { route = Just newRoute }
                pure (Just a)
