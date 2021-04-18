module Turing.Page.Spec where

import Prelude

import Turing.Capability.Navigate (class Navigate)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe(..))
import Effect.Console as Console
import Turing.Utils.Random (randomString)
import Data.Newtype (wrap)
import Data.Array
import Turing.Data.Spec (SpecId, Spec)
import Turing.Data.Spec as Spec
import Slug (Slug)

data Action =
    NewSpec

type State =
    { specs :: Array Spec
    }

type Input =
  { id :: SpecId
  }

type Message = Void

component
    :: forall q m
    . MonadAff m
    => Navigate m
    => H.Component HH.HTML q Input Message m
component = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
    where
    render state =
        HH.div_
            [ HH.button
                [ HE.onClick $ const $ Just NewSpec ]
                [ HH.text "New spec" ]
            ]

    handleAction :: forall slots. Action -> H.HalogenM State Action slots Message m Unit
    handleAction action =
        case action of
            NewSpec -> do pure unit
    initialState :: Input -> State
    initialState _ = { specs: [] }

