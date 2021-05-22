--| See https://firebase.google.com/docs/reference/js/firebase.firestore.CollectionReference
module Turing.Firebase.CollectionReference where

import Prelude
import Effect (Effect)
import Control.Promise (Promise)
import Turing.Firebase.DocumentReference (DocumentReference)
import Turing.Firebase.GetOptions (GetOptions)
import Turing.Firebase.QuerySnapshot (QuerySnapshot)

foreign import data CollectionReference :: Type -> Type

type DocumentPath = String

foreign import doc :: forall t. CollectionReference t -> DocumentPath -> Effect (DocumentReference t)

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.CollectionReference#get
foreign import get :: forall t. CollectionReference t -> GetOptions -> Effect (Promise (QuerySnapshot t))
