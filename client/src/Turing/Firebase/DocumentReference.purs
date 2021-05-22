module Turing.Firebase.DocumentReference where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Turing.Firebase.DocumentSnapshot (DocumentSnapshot)
import Turing.Firebase.GetOptions (GetOptions)

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentReference
foreign import data DocumentReference :: Type -> Type

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentReference#get
foreign import get :: forall t. DocumentReference t -> Maybe GetOptions -> Effect (Promise (DocumentSnapshot t))

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentReference#set
foreign import setImpl :: forall t. DocumentReference t -> t -> Effect (Promise Unit)

set :: forall t. DocumentReference t -> t -> Aff Unit
set ref value = toAffE $ setImpl ref value

get_ :: forall t. DocumentReference t -> Aff (DocumentSnapshot t)
get_ ref = toAffE $ get ref Nothing
