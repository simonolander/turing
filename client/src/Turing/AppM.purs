module Turing.AppM where

import Control.Monad.Reader.Class (asks)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, runReaderT)
import Control.Parallel (class Parallel, parallel, sequential)
import Control.Promise (toAff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, ParAff)
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
import Turing.Firebase.Firestore as Firestore
import Type.Equality (class TypeEquals, from)
import Control.Monad.Except (runExcept)
import Data.Nullable as Nullable
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)

import Foreign
import Foreign.Index

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
        documentSnapshot <- H.liftAff $ toAff documentSnapshotPromise
        maybeDocumentSnapshotData <- liftEffect $ Nullable.toMaybe
            <$> DocumentSnapshot._data documentSnapshot Nothing
        pure case maybeDocumentSnapshotData of
            Nothing -> Right Nothing
            Just documentSnapshotData -> decodeJson documentSnapshotData

    saveSpec spec = do
        setPromise <- liftEffect do
            firestore <- Firestore.firestore
            collectionRef <- Firestore.collection firestore "specs"
            documentRef <- CollectionReference.doc collectionRef spec.id
            DocumentReference.set documentRef (encodeJson spec)
        H.liftAff $ toAff setPromise
