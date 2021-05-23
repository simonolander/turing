module Turing.Capability.ManageProgram where

import Prelude

import Control.Monad.Reader.Trans (lift)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Halogen (HalogenM)
import Turing.Data.Program (Program, ProgramId)

class Monad m <= ManageProgram m where
    getProgram :: ProgramId -> m (Either String (Maybe Program))
    getPrograms :: m (Either String (Array Program))
    saveProgram :: Program -> m (Either String Unit)

instance manageProgramHalogenM :: ManageProgram m => ManageProgram ( HalogenM state action slots output m ) where
    getProgram = lift <<< getProgram
    getPrograms = lift getPrograms
    saveProgram = lift <<< saveProgram
