module Turing.Firebase.Firestore where

import Prelude
import Effect (Effect)
import Control.Promise (Promise)

foreign import data Firestore :: Type
foreign import data CollectionReference :: Type -> Type
foreign import data DocumentData :: Type
foreign import data DocumentReference :: Type -> Type
foreign import data QuerySnapshot :: Type -> Type
foreign import data GetOptions :: Type

foreign import firestore :: Effect Firestore
foreign import collection :: Firestore -> Effect (CollectionReference DocumentData)
foreign import get :: forall t. CollectionReference t -> Effect (Promise (QuerySnapshot t))
foreign import add :: forall t. CollectionReference t -> t -> Effect (Promise (DocumentReference t))
