module Turing.Data.Route where

import Prelude hiding ((/))
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Data.Show.Generic (genericShow)
import Turing.Data.Spec (SpecId)

data Route
    = Home
    | Specs
    | Settings
    | SpecEditor SpecId

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
instance showRoute :: Show Route where
    show = genericShow

route :: RouteDuplex' Route
route = root $ sum
    { "Home": noArgs
    , "Specs": "specs" / noArgs
    , "SpecEditor": "specs" / segment
    , "Settings": "settings" / noArgs
    }
