module Turing.Capability.Navigate where

import Prelude

import Control.Monad.Trans.Class (lift)
import Turing.Data.Route (Route)
import Halogen (HalogenM)

class Monad m <= Navigate m where
    navigate :: Route -> m Unit
    logout :: m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM state action slots output m) where
    navigate = lift <<< navigate
    logout = lift logout
