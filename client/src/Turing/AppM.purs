module Turing.AppM where

import Prelude
import Control.Monad.Reader.Class (asks)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, runReaderT)
import Control.Parallel (class Parallel, parallel, sequential)
import Control.Promise (toAff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, ParAff, try)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Prelude
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Turing.Capability.ManageSpec (class ManageSpec)
import Turing.Capability.Navigate (class Navigate)
import Turing.Data.Env (Env)
import Turing.Data.Route as Route
import Turing.Firebase.CollectionReference as CollectionReference
import Turing.Firebase.DocumentReference as DocumentReference
import Turing.Firebase.DocumentSnapshot as DocumentSnapshot
import Turing.Firebase.QuerySnapshot as QuerySnapshot
import Turing.Firebase.QueryDocumentSnapshot (QueryDocumentSnapshot)
import Turing.Firebase.QueryDocumentSnapshot as QueryDocumentSnapshot
import Turing.Firebase.Firestore as Firestore
import Type.Equality (class TypeEquals, from)
import Control.Monad.Except (runExcept)
import Data.Nullable as Nullable
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Effect.Exception as Ex
import Data.Bifunctor (lmap, rmap)
import Turing.Effect.Error (showError)
import Data.Traversable (for)

import Debug (traceM)
import Data.Argonaut.Core (Json)
import Turing.Data.Spec (Spec)
import Data.Traversable
import Unsafe.Coerce (unsafeCoerce)

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
    ask = AppM $ asks from

newtype ParAppM a
  = ParAppM (ReaderT Env ParAff a)

derive newtype instance functorParAppM :: Functor ParAppM
derive newtype instance applyParAppM :: Apply ParAppM
derive newtype instance applicativeParAppM :: Applicative ParAppM

instance parallelAppM :: Parallel ParAppM AppM where
    parallel (AppM readerT) = ParAppM (parallel readerT)
    sequential (ParAppM readerT) = AppM (sequential readerT)

instance navigateAppM :: Navigate AppM where
    navigate = liftEffect <<< setHash <<< print Route.route

instance manageSpecAppM :: ManageSpec AppM where
    getSpec specId = do
        documentSnapshotPromise <- liftEffect do
            firestore <- Firestore.firestore
            collectionRef <- Firestore.collection firestore "specs"
            documentRef <- CollectionReference.doc collectionRef specId
            DocumentReference.get documentRef Nothing
        result <- H.liftAff $ try $ toAff documentSnapshotPromise
        case result of
            Left error -> pure $ Left error
            Right documentSnapshot -> do
                maybeDocumentSnapshotData <- liftEffect $ Nullable.toMaybe
                    <$> DocumentSnapshot._data documentSnapshot Nothing
                pure case maybeDocumentSnapshotData of
                    Nothing -> Right Nothing
                    Just documentSnapshotData ->
                        decodeJson documentSnapshotData
                            # lmap show
                            # lmap Ex.error

    getSpecs = H.liftAff $ lmap showError <$> try Firestore.getSpecs

    saveSpec spec = do
        setPromise <- liftEffect do
            firestore <- Firestore.firestore
            collectionRef <- Firestore.collection firestore "specs"
            documentRef <- CollectionReference.doc collectionRef spec.id
            DocumentReference.set documentRef (encodeJson spec)
        H.liftAff $ try $ toAff setPromise
