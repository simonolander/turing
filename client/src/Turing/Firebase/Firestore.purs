module Turing.Firebase.Firestore where

import Effect (Effect)
import Turing.Firebase.CollectionReference (CollectionReference)
import Data.Argonaut.Core (Json)

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.Firestore
foreign import data Firestore :: Type

--| See https://firebase.google.com/docs/reference/js/firebase.firestore#callable
foreign import firestore :: Effect Firestore

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.Firestore#collection
foreign import collection :: Firestore -> CollectionPath -> Effect (CollectionReference Json)
type CollectionPath = String
