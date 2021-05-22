module Turing.Firebase.DocumentSnapshot where

import Effect (Effect)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Control.Promise (Promise)
import Turing.Firebase.SnapshotOptions (SnapshotOptions)

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentSnapshot
foreign import data DocumentSnapshot :: Type -> Type

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentSnapshot#id
foreign import id :: forall t. DocumentSnapshot t -> String

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentSnapshot#exists
foreign import exists :: forall t. DocumentSnapshot t -> Boolean

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentSnapshot#data
foreign import _data :: forall t. DocumentSnapshot t -> Maybe SnapshotOptions -> Effect (Nullable t)
