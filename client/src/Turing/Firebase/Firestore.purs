module Turing.Firebase.Firestore where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (catMaybes)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Turing.Data.Spec (Spec)
import Turing.Effect.Error (hushError)
import Turing.Firebase.CollectionReference (CollectionReference, getAff_)
import Turing.Firebase.QueryDocumentSnapshot (_data_)
import Turing.Firebase.QuerySnapshot (docs)

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.Firestore
foreign import data Firestore :: Type

--| See https://firebase.google.com/docs/reference/js/firebase.firestore#callable
foreign import firestore :: Effect Firestore

--| See https://firebase.google.com/docs/reference/js/firebase.firestore.Firestore#collection
foreign import collection :: Firestore -> CollectionPath -> Effect (CollectionReference Json)

type CollectionPath
  = String

specsCollection :: Effect (CollectionReference Json)
specsCollection = do
  fs <- firestore
  collection fs "specs"

--| Fetches all the specs from the database. Any error when decoding the specs result in an
--| error log, and the spec is subsequently ignored.
getSpecs :: Aff (Array Spec)
getSpecs = do
  querySnapshot <- liftEffect specsCollection >>= getAff_
  liftEffect do
    queryData <- sequence $ _data_ <$> docs querySnapshot
    maybeSpecs <- sequence $ hushError <$> decodeJson <$> queryData
    pure $ catMaybes maybeSpecs
