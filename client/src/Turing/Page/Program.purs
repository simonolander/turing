module Turing.Page.Program where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), fromEither)
import Turing.Capability.ManageProgram (class ManageProgram, getProgram)
import Turing.Capability.ManageSpec (class ManageSpec, getSpec)
import Turing.Component.Form.Program as PF
import Turing.Component.Html.Utility (remoteData)
import Turing.Data.Program (Program, ProgramId)
import Turing.Data.Spec (Spec)
import Turing.Component.Html.Utility (navbar)
import Data.Newtype (wrap)
import Turing.Component.Html.Utility (section_)
import Turing.Component.Html.Utility (container_)

type State
  = { programId :: ProgramId
    , program :: RemoteData String (Maybe Program)
    , spec :: RemoteData String (Maybe Spec)
    }

type Input
  = ProgramId

data Action
  = Initialize
  | HandleProgramForm Program

type Slots
  = ( formless :: PF.Slot Unit )

component ::
  forall query output m.
  ManageProgram m =>
  ManageSpec m =>
  MonadEffect m =>
  MonadAff m =>
  H.Component query Input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: Input -> State
  initialState programId =
    { programId
    , program: NotAsked
    , spec: NotAsked
    }

  render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
  render state = remoteData state.program "program" state.programId renderProgram
    where
    renderProgram :: Program -> HH.HTML (H.ComponentSlot Slots m Action) Action
    renderProgram program =
      HH.div_
        [ navbar
        , section_
            [ container_
                [ HH.h1
                    [ HP.classes $ wrap <$> [ "title" ] ]
                    [ HH.text "Program" ]
                , HH.h2
                    [ HP.classes $ wrap <$> [ "subtitle", "is-6", "is-family-monospace" ] ]
                    [ HH.text program.id ]
                , HH.h4
                    [ HP.classes $ wrap <$> [ "title", "is-6" ] ]
                    [ HH.text "Name" ]
                ]
            ]
        , remoteData state.spec "spec" program.specId
            (\spec -> HH.slot F._formless unit PF.component { program, spec } HandleProgramForm)
        ]

  eval :: H.HalogenQ query Action Input ~> H.HalogenM State Action Slots output m
  eval =
    H.mkEval
      H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    where
    handleAction :: Action -> H.HalogenM State Action Slots output m Unit
    handleAction Initialize = do
      H.modify_ _ { program = Loading }
      programId <- H.gets _.programId
      eitherProgram <- getProgram programId
      H.modify_ _ { program = fromEither eitherProgram }
      case eitherProgram of
        Right (Just program) -> do
          H.modify_ _ { spec = Loading }
          spec <- getSpec program.specId
          H.modify_ _ { spec = fromEither spec }
        _ -> pure unit

    handleAction (HandleProgramForm program) = pure unit
