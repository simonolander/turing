module Turing.Firebase.QueryDocumentSnapshot where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Turing.Firebase.SnapshotOptions (SnapshotOptions)

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.QueryDocumentSnapshot
foreign import data QueryDocumentSnapshot :: Type -> Type

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.QueryDocumentSnapshot#data
foreign import _data :: forall t. QueryDocumentSnapshot t -> Maybe SnapshotOptions -> Effect t

--| Same as _data, but skips the option to provide SnapshotOptions
_data_ :: forall t. QueryDocumentSnapshot t -> Effect t
_data_ queryDocumentSnapshot = _data queryDocumentSnapshot Nothing
