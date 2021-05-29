module Turing.Page.Specs where

import Prelude
import Data.Array (singleton)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), fromEither, isLoading)
import Turing.AppM (AppM)
import Turing.Capability.ManageSpec (getSpecs, saveSpec)
import Turing.Capability.Navigate (navigate)
import Turing.Component.Html.Utility (safeHref, whenFailure)
import Turing.Data.Route (Route(..))
import Turing.Data.Spec (Spec, mkSpec)

type State
  = { newSpecCreation :: RemoteData String Void
    , specs :: RemoteData String (Array Spec)
    }

data Action
  = Initialize
  | ClickedNewSpec

type Slots :: forall k. Row k
type Slots
  = ()

type Query :: forall k. k -> Type
type Query
  = Const Void

type Input
  = Unit

type Output
  = Void

component :: H.Component Query Input Output AppM
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: Input -> State
  initialState _input =
    { newSpecCreation: NotAsked
    , specs: NotAsked
    }

  render :: State -> HH.HTML (H.ComponentSlot Slots AppM Action) Action
  render state =
    HH.div_
      [ HH.h1_ [ HH.text "Specs" ]
      , HH.button
          [ HE.onClick $ const ClickedNewSpec
          , HP.disabled $ isLoading state.newSpecCreation
          ]
          [ HH.text "New spec" ]
      , whenFailure state.newSpecCreation
          $ const
          $ HH.p_
              [ HH.text "Something went wrong when creating the spec. Check the logs for details."
              ]
      , renderSpecs state.specs
      ]
    where
    renderSpecs specsRD =
      HH.div_
        [ HH.h2_ [ HH.text "Specs" ]
        , case specsRD of
            NotAsked -> HH.p_ [ HH.text "Not asked" ]
            Loading -> HH.p_ [ HH.text "Loading specs" ]
            Failure error -> HH.p_ [ HH.text error ]
            Success specs -> HH.ul_ $ renderSpec <$> specs
              where
              renderSpec spec =
                HH.li_ $ singleton
                  $ HH.a
                      [ safeHref $ SpecEditor spec.id ]
                      [ HH.text spec.name ]
        ]

  eval :: H.HalogenQ Query Action Input ~> H.HalogenM State Action Slots Output AppM
  eval =
    H.mkEval
      $ H.defaultEval
          { handleAction = handleAction
          , initialize = Just Initialize
          }
    where
    handleAction :: Action -> H.HalogenM State Action Slots Output AppM Unit
    handleAction Initialize = do
      H.modify_ _ { specs = Loading }
      result <- getSpecs
      H.modify_ _ { specs = fromEither result }

    handleAction ClickedNewSpec = do
      H.modify_ _ { newSpecCreation = Loading }
      spec <- H.liftEffect mkSpec
      result <- saveSpec spec
      case result of
        Left error -> H.modify_ _ { newSpecCreation = Failure error }
        Right _ -> navigate $ SpecEditor spec.id
