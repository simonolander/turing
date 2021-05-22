module Turing.Firebase.QueryDocumentSnapshot where

import Data.Maybe (Maybe)
import Effect (Effect)
import Turing.Firebase.SnapshotOptions (SnapshotOptions)

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.QueryDocumentSnapshot
foreign import data QueryDocumentSnapshot :: Type -> Type

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.QueryDocumentSnapshot#data
foreign import _data :: forall t. QueryDocumentSnapshot t -> Maybe SnapshotOptions -> Effect t
