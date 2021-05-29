-- | This page displays a given spec. If the spec is published, the user can
-- | browse their created programs for the given spec, and create new programs.
-- |
-- | If the spec is unpublished, and the user is the creator of the spec, the user
-- | can edit the spec, try it out, and ultimately publish the spec.
-- |
-- | If the spec is published, and the user has published at least one program that
-- | solves the spec, the user can view leaderboards for the spec.
module Turing.Page.Spec where

import Prelude

import Data.Array (find)
import Data.Either (isRight)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.RemoteData (RemoteData(..), fromEither)
import Turing.Capability.ManageProgram (class ManageProgram, saveProgram)
import Turing.Capability.Navigate (class Navigate, navigate)
import Turing.Component.Html.Utility (remoteData)
import Turing.Data.Campaign (campaigns)
import Turing.Data.Program (mkProgram)
import Turing.Data.Spec (Spec, SpecId)
import Turing.Data.Route (Route(..))

type State =
    { specId :: SpecId
    , spec :: RemoteData String (Maybe Spec)
    , newProgram :: RemoteData String Unit
    }

type Input = SpecId

data Action
    = Initialize
    | ClickedCreateProgram

component
    :: forall query output m
     . MonadEffect m
    => ManageProgram m
    => Navigate m
    => H.Component query Input output m
component = H.mkComponent { initialState, render, eval }
    where
    initialState :: Input -> State
    initialState specId =
        { specId
        , spec: NotAsked
        , newProgram: NotAsked
        }

    render :: forall slots. State -> HH.HTML (H.ComponentSlot slots m Action) Action
    render state =
        remoteData state.spec "spec" state.specId renderSpec
        where
        renderSpec :: Spec -> HH.HTML (H.ComponentSlot slots m Action) Action
        renderSpec spec =
            HH.div_
                [ HH.h1_ [ HH.text spec.name ]
                , HH.p_
                    [ HH.text
                        """
                        Busy beavers try to light as many lightbulbs as possible before terminating.
                        All lightbulbs on the tape are turned off.
                        """
                    ]
                , HH.h3_ [ HH.text "Maximum number of cards" ]
                , HH.p_ [ HH.text $ show spec.maxNumberOfCards ]
                , HH.h2_ [ HH.text "My programs" ]
                , HH.button
                    [ HE.onClick $ const ClickedCreateProgram ]
                    [ HH.text "Create new program" ]
                ]

    eval :: forall slots. H.HalogenQ query Action Input ~> H.HalogenM State Action slots output m
    eval = H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
        where
        handleAction :: Action -> H.HalogenM State Action slots output m Unit
        handleAction Initialize = do
            specId <- H.gets _.specId
            case find (\ spec -> spec.id == specId) $ join $ _.specs <$> campaigns of
                Just spec -> H.modify_ _ { spec = Success $ Just spec }
                Nothing -> H.modify_ _ { spec = Success Nothing }
        handleAction ClickedCreateProgram = do
            specRD <- H.gets _.spec
            case specRD of
                Success (Just spec) -> do
                    program <- H.liftEffect $ mkProgram spec
                    H.modify_ _ { newProgram = Loading }
                    result <- saveProgram program
                    H.modify_ _ { newProgram = fromEither result }
                    when (isRight result) do
                        navigate $ Program program.id
                _ -> pure unit
