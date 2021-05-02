--| See https://firebase.google.com/docs/reference/js/firebase.firestore.DocumentReference
module Turing.Firebase.DocumentReference where

import Prelude
import Effect (Effect)
import Control.Promise (Promise)
import Data.Maybe (Maybe)
import Turing.Firebase.DocumentSnapshot (DocumentSnapshot)

foreign import data DocumentReference :: Type -> Type
foreign import data GetOptions :: Type

foreign import get :: forall t. DocumentReference t -> Maybe GetOptions -> Effect (Promise (DocumentSnapshot t))
foreign import set :: forall t. DocumentReference t -> t -> Effect (Promise Unit)
