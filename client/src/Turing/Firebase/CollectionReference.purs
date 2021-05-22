--| See https://firebase.google.com/docs/reference/js/firebase.firestore.CollectionReference
module Turing.Firebase.CollectionReference where

import Prelude
import Effect (Effect)
import Control.Promise (Promise)
import Turing.Firebase.DocumentReference (DocumentReference)

foreign import data CollectionReference :: Type -> Type

type DocumentPath = String

foreign import doc :: forall t. CollectionReference t -> DocumentPath -> Effect (DocumentReference t)
