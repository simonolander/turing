-- | The registration form allows new users to sign up to the Turing service and authenticate
-- | their session.
module Turing.Page.Credits where

import Prelude

import Turing.Capability.Navigate (class Navigate)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

type Action = Unit

component
  :: forall q o m
   . MonadAff m
  => Navigate m
  => H.Component HH.HTML q Unit o m
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval H.defaultEval
  }
  where
  render _ =
    HH.text "Credits"
