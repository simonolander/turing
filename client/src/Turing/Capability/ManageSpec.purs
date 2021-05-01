module Turing.Capability.ManageSpec where

import Prelude
import Control.Monad.Reader.Trans (lift)
import Halogen (HalogenM)
import Turing.Data.Spec (Spec, SpecId)
import Data.Maybe (Maybe)
import Foreign (MultipleErrors)
import Data.Either (Either)

class Monad m <= ManageSpec m where
    getSpec :: SpecId -> m (Either MultipleErrors (Maybe Spec))

instance manageSpecHalogenM :: ManageSpec m => ManageSpec ( HalogenM state action slots output m ) where
    getSpec = lift <<< getSpec
