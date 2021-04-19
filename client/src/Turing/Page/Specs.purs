-- | The credits page display everyone that helped out making this application
-- |
-- |  - IntelliJ plugin team
-- |  - Halogen team
-- |  - Halogen real world
-- |
module Turing.Page.Specs where

import Prelude

import Turing.Capability.Navigate (class Navigate, navigate)
import Turing.Capability.Resource.Spec (class ManageSpec, createSpec)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe(..))
import Effect.Console as Console
import Turing.Utils.Random (randomString)
import Turing.Data.Route as Route
import Data.Array
import Data.Newtype (wrap)
import Turing.Data.Spec as Spec
import Data.Show (show)

data Action =
    NewSpec

type State =
    { specs :: Array Spec.Spec
    }

type Input = Unit
type Message = Void

component
    :: forall q m
    . MonadAff m
    => Navigate m
    => ManageSpec m
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
            , HH.ul_ $
                state.specs
                    <#> (\spec -> show spec.id)
                    <#> (\id -> HH.li_ [ HH.text id ])
            ]

    handleAction :: forall slots. Action -> H.HalogenM State Action slots Message m Unit
    handleAction action =
        case action of
            NewSpec -> do
                state <- H.get
                str <- H.liftEffect $ randomString 6
                let id = wrap str
                createSpec $ Spec.createSpec id
                navigate (Route.Spec id)

    initialState :: Input -> State
    initialState _ = { specs: [] }

