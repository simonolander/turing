module Turing.Firebase.Firestore where

import Effect (Effect)
import Turing.Firebase.CollectionReference (CollectionReference, DocumentData)
import Foreign (Foreign)

foreign import data Firestore :: Type

type CollectionPath = String

foreign import firestore :: Effect Firestore
foreign import collection :: Firestore -> CollectionPath -> Effect (CollectionReference Foreign)
