module Turing.Firebase.QuerySnapshot where

import Effect (Effect)
import Turing.Firebase.QueryDocumentSnapshot (QueryDocumentSnapshot)

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.QuerySnapshot
foreign import data QuerySnapshot :: Type -> Type

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.QuerySnapshot#docs
foreign import docs :: forall t. QuerySnapshot t -> Array (QueryDocumentSnapshot t)
