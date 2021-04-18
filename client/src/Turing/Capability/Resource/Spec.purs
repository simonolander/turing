module Turing.Capability.Resource.Spec where

import Prelude
import Halogen (HalogenM, lift)
import Turing.Data.Spec

class Monad m <= ManageSpec m where
    createSpec :: Spec -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageSpecHalogenM :: ManageSpec m => ManageSpec (HalogenM st act slots msg m) where
  createSpec = lift <<< createSpec
