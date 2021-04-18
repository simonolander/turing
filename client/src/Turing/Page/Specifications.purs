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
import Turing.Utils.Random (randomString)
import Data.Array
import Turing.Data.Specification as Spec

data Action =
    NewSpecification

type State =
    { specifications :: Array Spec.Specification
    }

type Input = Unit
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
                [ HE.onClick $ const $ Just NewSpecification ]
                [ HH.text "New spec" ]
            , HH.ul_ $
                state.specifications
                    <#> (\spec -> spec.id)
                    <#> (\id -> HH.li_ [ HH.text id ])
            ]

    handleAction :: forall slots. Action -> H.HalogenM State Action slots Message m Unit
    handleAction action =
        case action of
            NewSpecification -> do
                state <- H.get
                id <- H.liftEffect $ randomString 6
                H.modify_ _ { specifications = Spec.createSpecification id : state.specifications }

    initialState :: Input -> State
    initialState _ = { specifications: [] }

