module Turing.Capability.ManageSpec where

import Prelude
import Control.Monad.Reader.Trans (lift)
import Halogen (HalogenM)
import Turing.Data.Spec (Spec, SpecId)
import Data.Maybe (Maybe)
import Data.Either (Either)
import Effect.Exception (Error)

class Monad m <= ManageSpec m where
    getSpec :: SpecId -> m (Either Error (Maybe Spec))
    saveSpec :: Spec -> m (Either Error Unit)

instance manageSpecHalogenM :: ManageSpec m => ManageSpec ( HalogenM state action slots output m ) where
    getSpec = lift <<< getSpec
    saveSpec = lift <<< saveSpec
