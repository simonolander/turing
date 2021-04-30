module Turing.Capability.ManageSpec where

import Prelude
import Control.Monad.Reader.Trans (lift)
import Halogen (HalogenM)
import Turing.Data.Route (Route)

class Monad m <= ManageSpec m where
    manageSpec :: Route -> m Unit

instance manageSpecHalogenM :: ManageSpec m => ManageSpec ( HalogenM state action slots output m ) where
    manageSpec = lift <<< manageSpec
