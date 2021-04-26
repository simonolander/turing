module Turing.Component.Router where

import Prelude
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Data.Maybe (Maybe(..), fromMaybe)
import Turing.Capability.Navigate (class Navigate)
import Turing.Data.Route (Route(..), route)
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
    ( home :: forall query. H.Slot query Void Unit )

type Input = Unit

type Output = Void

component :: forall m.
    MonadEffect m =>
    Navigate m =>
    H.Component Query Input Output m
component = H.mkComponent { initialState, render, eval }
    where
    initialState :: Input -> State
    initialState = const { route: Nothing }

    render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
    render state = HH.text (show state)

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
                H.modify_ _ { route = Just $ fromMaybe Home initialRoute }

        handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Output m (Maybe a)
        handleQuery query = case query of
            Navigate newRoute a -> do
                { route } <- H.get
                when (route /= Just newRoute) do
                    H.modify_ _ { route = Just newRoute }
                pure (Just a)
