module Turing.Capability.Navigate where

import Prelude
import Control.Monad.Reader.Trans (lift)
import Halogen (HalogenM)
import Turing.Data.Route (Route)

class Monad m <= Navigate m where
    navigate :: Route -> m Unit

--instance navigateHalogenM :: Navigate ( HalogenM state action slots output m ) where
--    navigate = lift <<< navigate
