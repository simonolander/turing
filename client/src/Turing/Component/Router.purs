module Turing.Component.Router where

import Prelude
import Data.Const (Const)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex (parse)
import Routing.Hash (getHash)
import Turing.AppM (AppM)
import Turing.Capability.Navigate (navigate)
import Turing.Data.Route (Route(..), route)
import Turing.Page.Campaign as Campaign
import Turing.Page.Home as Home
import Turing.Page.Play as Play
import Turing.Page.Program as Program
import Turing.Page.Settings as Settings
import Turing.Page.Spec as Spec
import Turing.Page.SpecEditor as SpecEditor
import Turing.Page.Specs as Specs
import Type.Proxy (Proxy(..))

type State
  = { route :: Maybe Route
    }

data Query a
  = Navigate Route a

data Action
  = Initialize

type Slots
  = ( home :: H.Slot (Const Void) Void Unit
    , specs :: H.Slot (Const Void) Void Unit
    , specEditor :: H.Slot (Const Void) Void Unit
    , spec :: H.Slot (Const Void) Void Unit
    , settings :: H.Slot (Const Void) Void Unit
    , play :: H.Slot (Const Void) Void Unit
    , campaign :: H.Slot (Const Void) Void Unit
    , program :: H.Slot (Const Void) Void Unit
    )

type Input
  = Unit

type Output
  = Void

component :: H.Component Query Input Output AppM
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: Input -> State
  initialState = const { route: Nothing }

  render :: State -> HH.HTML (H.ComponentSlot Slots AppM Action) Action
  render state = case state.route of
    Just Home -> HH.slot (Proxy :: _ "home") unit Home.component unit absurd
    Just Specs -> HH.slot (Proxy :: _ "specs") unit Specs.component unit absurd
    Just Play -> HH.slot (Proxy :: _ "play") unit Play.component unit absurd
    Just Settings -> HH.slot (Proxy :: _ "settings") unit Settings.component unit absurd
    Just (SpecEditor specId) -> HH.slot (Proxy :: _ "specEditor") unit SpecEditor.component specId absurd
    Just (Spec specId) -> HH.slot (Proxy :: _ "spec") unit Spec.component specId absurd
    Just (Campaign campaignId) -> HH.slot (Proxy :: _ "campaign") unit Campaign.component campaignId absurd
    Just (Program programId) -> HH.slot (Proxy :: _ "program") unit Program.component programId absurd
    Nothing -> HH.text "404 Not found"

  eval :: H.HalogenQ Query Action Input ~> H.HalogenM State Action Slots Output AppM
  eval =
    H.mkEval
      H.defaultEval
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
