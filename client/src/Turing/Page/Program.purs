module Turing.Page.Program where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..), fromEither)
import Turing.Capability.ManageProgram (class ManageProgram, getProgram)
import Turing.Capability.ManageSpec (class ManageSpec, getSpec)
import Turing.Component.Html.Utility (remoteData)
import Turing.Data.Program (Program, ProgramId)
import Turing.Data.Spec (Spec)
import Data.Either (Either(..))

type State =
    { programId :: ProgramId
    , program :: RemoteData String (Maybe Program)
    , spec :: RemoteData String (Maybe Spec)
    }

type Input = ProgramId

data Action
    = Initialize

component
    :: forall query output m
     . ManageProgram m
    => ManageSpec m
    => H.Component query Input output m
component = H.mkComponent { initialState, render, eval }
    where
    initialState :: Input -> State
    initialState programId =
        { programId
        , program: NotAsked
        , spec: NotAsked
        }

    render :: forall slots. State -> HH.HTML (H.ComponentSlot slots m Action) Action
    render state = remoteData state.program "program" state.programId renderProgram
        where
        renderProgram :: Program -> HH.HTML (H.ComponentSlot slots m Action) Action
        renderProgram program =
            HH.div_
                [ HH.h1_ [ HH.text program.name ]
                ]

    eval :: forall slots. H.HalogenQ query Action Input ~> H.HalogenM State Action slots output m
    eval = H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
        where
        handleAction :: Action -> H.HalogenM State Action slots output m Unit
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
