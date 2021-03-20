-- | This module exports a pure HTML function to render a consistent header throughout the app.
module Turing.Component.HTML.Header where

import Prelude

import Turing.Component.HTML.Utils (css, maybeElem, safeHref, whenElem)
import Turing.Data.Avatar as Avatar
import Turing.Data.Profile (ProfileRep)
import Turing.Data.Route (Route(..))
import Turing.Data.Username as Username
import Data.Maybe (Maybe, isNothing, isJust)
import Data.Monoid (guard)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | Our header will be a pure render function, but we'll require a route as an argument so we can
-- | judge whether a link should display active or not. We'll allow for any profile record type so
-- | long as it has our core fields -- this makes the header reusable across pages despite which
-- | variation on `Profile` they use.
header :: forall i p r. Maybe { | ProfileRep r } -> Route -> HH.HTML i p
header currentUser route =
  HH.nav
    [ css "navbar" ]
    [ HH.a
      [ css "navbar-brand"
      , safeHref Home
      ]
      [ HH.text "Turing" ]
    , HH.div
          [ css "navbar-end" ]
          [ maybeElem currentUser \profile ->
              HH.text $ Username.toString profile.username
          , whenElem (isNothing currentUser) \_ ->
              HH.a
                  []
                  [ HH.text "Sign in" ]
          ]
    ]
