--| See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentSnapshot
module Turing.Firebase.DocumentSnapshot where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Control.Promise (Promise)

foreign import data DocumentSnapshot :: Type -> Type
foreign import data SnapshotOptions :: Type

foreign import id :: forall t. DocumentSnapshot t -> Effect String
foreign import exists :: forall t. DocumentSnapshot t -> Effect Boolean
foreign import _data :: forall t. DocumentSnapshot t -> Maybe SnapshotOptions -> Effect (Nullable t)
