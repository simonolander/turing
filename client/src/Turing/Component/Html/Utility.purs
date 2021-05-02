module Turing.Component.Html.Utility where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Turing.Data.Route (Route, route)
import Routing.Duplex (print)

safeHref :: forall r i. Route -> HH.IProp ( href :: String | r) i
safeHref = HP.href <<< append "#" <<< print route
