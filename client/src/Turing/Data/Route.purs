module Turing.Data.Route where

import Prelude
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Data.Show.Generic (genericShow)
import Data.Eq (class Eq)

data Route
    = Home

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
instance showRoute :: Show Route where
    show = genericShow

route :: RouteDuplex' Route
route = root $ sum
    { "Home": noArgs
    }
