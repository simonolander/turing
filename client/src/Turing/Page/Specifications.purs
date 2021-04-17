-- | The credits page display everyone that helped out making this application
-- |
-- |  - IntelliJ plugin team
-- |  - Halogen team
-- |  - Halogen real world
-- |
module Turing.Page.Specifications where

import Prelude

import Turing.Capability.Navigate (class Navigate)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe(..))
import Effect.Console as Console

data Action =
    NewSpecification

type Input = Unit
type State = Unit
type Message = Void

component
    :: forall q m
    . MonadAff m
    => Navigate m
    => H.Component HH.HTML q Input Message m
component = H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
    where
    render _ =
        HH.div_
            [ HH.button
                [ HE.onClick $ const $ Just NewSpecification ]
                [ HH.text "New spec" ]
            ]

    handleAction :: forall slots. Action -> H.HalogenM State Action slots Message m Unit
    handleAction action =
        case action of
            NewSpecification -> do
                H.liftEffect $ Console.log "Hello"

