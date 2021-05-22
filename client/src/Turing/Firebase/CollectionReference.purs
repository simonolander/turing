module Turing.Firebase.CollectionReference where

import Effect (Effect)
import Control.Promise (Promise)
import Turing.Firebase.DocumentReference (DocumentReference)
import Turing.Firebase.GetOptions (GetOptions)
import Turing.Firebase.QuerySnapshot (QuerySnapshot)

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.CollectionReference
foreign import data CollectionReference :: Type -> Type


--| See https://firebase.google.com/docs/reference/js/firebase.firestore.CollectionReference#doc
foreign import doc :: forall t. CollectionReference t -> DocumentPath -> Effect (DocumentReference t)
type DocumentPath = String

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.CollectionReference#get
foreign import get :: forall t. CollectionReference t -> GetOptions -> Effect (Promise (QuerySnapshot t))
