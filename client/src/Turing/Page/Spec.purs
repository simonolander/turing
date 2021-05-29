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
import Data.Newtype (wrap)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), fromEither)
import Turing.Capability.ManageProgram (class ManageProgram, saveProgram)
import Turing.Capability.Navigate (class Navigate, navigate)
import Turing.Component.Html.Utility (container_, navbar, remoteData, section_)
import Turing.Data.Campaign (campaigns)
import Turing.Data.Program (Program, mkProgram)
import Turing.Data.Route (Route(..))
import Turing.Data.Spec (Spec, SpecId)

type State
  = { specId :: SpecId
    , spec :: RemoteData String (Maybe Spec)
    , newProgram :: RemoteData String Unit
    , programs :: RemoteData String (Program)
    }

type Input
  = SpecId

data Action
  = Initialize
  | ClickedCreateProgram

component ::
  forall query output m.
  MonadEffect m =>
  ManageProgram m =>
  Navigate m =>
  H.Component query Input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: Input -> State
  initialState specId =
    { specId
    , spec: NotAsked
    , newProgram: NotAsked
    , programs: NotAsked
    }

  render :: forall slots. State -> HH.HTML (H.ComponentSlot slots m Action) Action
  render state = remoteData state.spec "spec" state.specId renderSpec
    where
    renderSpec :: Spec -> HH.HTML (H.ComponentSlot slots m Action) Action
    renderSpec spec =
      HH.div_
        [ navbar
        , section_
            [ HH.h1
                [ HP.classes $ wrap <$> [ "title", "has-text-centered", "is-1" ] ]
                [ HH.text spec.name ]
            ]
        , container_
            [ HH.h4
                [ HP.classes $ wrap <$> [ "title", "is-4" ] ]
                [ HH.text "Description" ]
            , HH.p
                [ HP.classes $ wrap <$> [ "content" ] ]
                [ HH.text "Busy beavers try to light as many lightbulbs as possible before terminating. All lightbulbs on the tape are turned off." ]
            , HH.h4
                [ HP.classes $ wrap <$> [ "title", "is-4" ] ]
                [ HH.text "Maximum number of cards" ]
            , HH.p
                [ HP.classes $ wrap <$> [ "content" ] ]
                [ HH.text $ show spec.maxNumberOfCards ]
            , HH.h4
                [ HP.classes $ wrap <$> [ "title", "is-4" ] ]
                [ HH.text "My programs" ]
            , HH.button
                [ HE.onClick $ const ClickedCreateProgram
                , HP.classes $ wrap <$> [ "button", "is-primary" ]
                ]
                [ HH.text "Create new program" ]
            , renderPrograms
            ]
        ]

    renderPrograms =
      HH.table
        [ HP.class_ $ wrap "section table" ]
        [ HH.thead_
            [ HH.tr_
                [ HH.th_ [ HH.p_ [ HH.text "Id" ] ]
                , HH.th_ [ HH.p_ [ HH.text "Name" ] ]
                , HH.th_ [ HH.p_ [ HH.text "Open" ] ]
                ]
            ]
        , HH.tbody_
            [ HH.tr_
                [ HH.td_ [ HH.p_ [ HH.text "1" ] ]
                , HH.td_ [ HH.p_ [ HH.text "My program" ] ]
                , HH.td_ [ HH.a [ HP.classes $ wrap <$> [ "button", "is-outlined", "is-info", "is-small" ] ] [ HH.text "Open" ] ]
                ]
            , HH.tr_
                [ HH.td_ [ HH.p_ [ HH.text "2" ] ]
                , HH.td_ [ HH.p_ [ HH.text "My program" ] ]
                , HH.td_ [ HH.a [ HP.classes $ wrap <$> [ "button", "is-outlined", "is-info", "is-small" ] ] [ HH.text "Open" ] ]
                ]
            , HH.tr_
                [ HH.td_ [ HH.p_ [ HH.text "3" ] ]
                , HH.td_ [ HH.p_ [ HH.text "My program" ] ]
                , HH.td_ [ HH.a [ HP.classes $ wrap <$> [ "button", "is-outlined", "is-info", "is-small" ] ] [ HH.text "Open" ] ]
                ]
            , HH.tr_
                [ HH.td_ [ HH.p_ [ HH.text "4" ] ]
                , HH.td_ [ HH.p_ [ HH.text "My program" ] ]
                , HH.td_ [ HH.a [ HP.classes $ wrap <$> [ "button", "is-outlined", "is-info", "is-small" ] ] [ HH.text "Open" ] ]
                ]
            ]
        ]

  eval :: forall slots. H.HalogenQ query Action Input ~> H.HalogenM State Action slots output m
  eval =
    H.mkEval
      H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    where
    handleAction :: Action -> H.HalogenM State Action slots output m Unit
    handleAction Initialize = do
      specId <- H.gets _.specId
      case find (\spec -> spec.id == specId) $ join $ _.specs <$> campaigns of
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
