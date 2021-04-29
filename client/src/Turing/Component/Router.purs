module Turing.Component.Router where

import Prelude
import Effect.Class (class MonadEffect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Data.Maybe (Maybe(..), fromMaybe)
import Turing.Capability.Navigate (class Navigate, navigate)
import Turing.Data.Route (Route(..), route)
import Turing.Page.Home as Home
import Turing.Page.Specs as Specs
import Turing.Page.Settings as Settings
import Type.Proxy (Proxy(..))
import Routing.Hash (getHash)
import Routing.Duplex (parse)
import Data.Either (hush)

type State =
    { route :: Maybe Route
    }

data Query a = Navigate Route a

data Action
    = Initialize

type Slots =
    ( home :: forall query. H.Slot query Void Unit
    , specs :: forall query. H.Slot query Void Unit
    , settings :: forall query. H.Slot query Void Unit
    )

type Input = Unit

type Output = Void

component :: forall m.
    MonadEffect m =>
    MonadAff m =>
    Navigate m =>
    H.Component Query Input Output m
component = H.mkComponent { initialState, render, eval }
    where
    initialState :: Input -> State
    initialState = const { route: Nothing }

    render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
    render state =
        case state.route of
            Just Home -> HH.slot (Proxy :: _ "home") unit Home.component unit absurd
            Just Specs -> HH.slot (Proxy :: _ "specs") unit Specs.component unit absurd
            Just Settings -> HH.slot (Proxy :: _ "settings") unit Settings.component unit absurd
            Nothing -> HH.text "404 Not found"

    eval :: H.HalogenQ Query Action Input ~> H.HalogenM State Action Slots Output m
    eval = H.mkEval H.defaultEval
        { initialize = initialize
        , handleAction = handleAction
        , handleQuery = handleQuery
        }
        where
        initialize :: Maybe Action
        initialize = Just Initialize

        handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
        handleAction action = case action of
            Initialize -> do
                initialRoute <- H.liftEffect $ hush <<< (parse route) <$> getHash
                navigate $ fromMaybe Home initialRoute

        handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Output m (Maybe a)
        handleQuery query = case query of
            Navigate newRoute a -> do
                { route } <- H.get
                when (route /= Just newRoute) do
                    H.modify_ _ { route = Just newRoute }
                pure (Just a)
