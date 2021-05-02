module Turing.Component.Html.Utility where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Turing.Data.Route (Route, route)
import Routing.Duplex (print)
import Network.RemoteData (RemoteData(..))

safeHref :: forall r i. Route -> HH.IProp ( href :: String | r) i
safeHref = HP.href <<< append "#" <<< print route

whenFailure :: forall e a w i. RemoteData e a -> (e -> HH.HTML w i) -> HH.HTML w i
whenFailure (Failure e) f = f e
whenFailure _ _ = HH.text ""
