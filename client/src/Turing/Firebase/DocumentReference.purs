module Turing.Firebase.DocumentReference where

import Prelude (Unit)
import Effect (Effect)
import Control.Promise (Promise)
import Data.Maybe (Maybe)
import Turing.Firebase.DocumentSnapshot (DocumentSnapshot)
import Turing.Firebase.GetOptions (GetOptions)

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentReference
foreign import data DocumentReference :: Type -> Type

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentReference#get
foreign import get :: forall t. DocumentReference t -> Maybe GetOptions -> Effect (Promise (DocumentSnapshot t))

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentReference#set
foreign import set :: forall t. DocumentReference t -> t -> Effect (Promise Unit)
