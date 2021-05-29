module Turing.Firebase.CollectionReference where

import Prelude
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Turing.Firebase.DocumentReference (DocumentReference)
import Turing.Firebase.GetOptions (GetOptions)
import Turing.Firebase.QuerySnapshot (QuerySnapshot)

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.CollectionReference
foreign import data CollectionReference :: Type -> Type

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.CollectionReference#doc
foreign import doc :: forall t. CollectionReference t -> DocumentPath -> Effect (DocumentReference t)

type DocumentPath
  = String

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.CollectionReference#get
foreign import get :: forall t. CollectionReference t -> Maybe GetOptions -> Effect (Promise (QuerySnapshot t))

get_ :: forall t. CollectionReference t -> Effect (Promise (QuerySnapshot t))
get_ collectionReference = get collectionReference Nothing

getAff_ :: forall t. CollectionReference t -> Aff (QuerySnapshot t)
getAff_ ref = toAffE $ get_ ref
