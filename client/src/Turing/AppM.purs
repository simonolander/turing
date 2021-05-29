module Turing.AppM where

import Prelude
import Control.Monad.Reader.Class (asks)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, runReaderT)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Data.Traversable (find)
import Effect.Aff (Aff, ParAff, try)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Turing.Capability.ManageProgram (class ManageProgram)
import Turing.Capability.ManageSpec (class ManageSpec)
import Turing.Capability.Navigate (class Navigate)
import Turing.Data.Campaign (campaigns)
import Turing.Data.Env (Env)
import Turing.Data.Route as Route
import Turing.Effect.Error (showError)
import Turing.Firebase.CollectionReference as CollectionReference
import Turing.Firebase.DocumentReference as DocumentReference
import Turing.Firebase.DocumentSnapshot as DocumentSnapshot
import Turing.Firebase.Firestore as Firestore
import Type.Equality (class TypeEquals, from)

newtype AppM a
  = AppM (ReaderT Env Aff a)

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
  getSpec specId = case find (\spec -> spec.id == specId) $ join $ _.specs <$> campaigns of
    Just spec -> pure $ Right $ Just spec
    Nothing -> do
      documentRef <-
        liftEffect do
          firestore <- Firestore.firestore
          collectionRef <- Firestore.collection firestore "specs"
          CollectionReference.doc collectionRef specId
      result <- H.liftAff $ try $ DocumentReference.get_ documentRef
      case result of
        Left error -> pure $ Left $ showError $ error
        Right documentSnapshot -> do
          maybeDocumentSnapshotData <-
            liftEffect $ Nullable.toMaybe
              <$> DocumentSnapshot._data documentSnapshot Nothing
          pure case maybeDocumentSnapshotData of
            Nothing -> Right Nothing
            Just documentSnapshotData ->
              decodeJson documentSnapshotData
                # lmap show
  getSpecs = H.liftAff $ lmap showError <$> try Firestore.getSpecs
  saveSpec spec =
    H.liftAff $ lmap showError
      <$> try do
          documentRef <-
            liftEffect do
              collectionRef <- Firestore.specsCollection
              CollectionReference.doc collectionRef spec.id
          DocumentReference.set documentRef (encodeJson spec)

instance manageProgramAppM :: ManageProgram AppM where
  saveProgram program =
    H.liftAff $ lmap showError
      <$> try do
          documentRef <-
            liftEffect do
              firestore <- Firestore.firestore
              collectionRef <- Firestore.collection firestore "programs"
              CollectionReference.doc collectionRef program.id
          DocumentReference.set documentRef (encodeJson program)
  getProgram programId = do
    documentRef <-
      liftEffect do
        firestore <- Firestore.firestore
        collectionRef <- Firestore.collection firestore "programs"
        CollectionReference.doc collectionRef programId
    result <-
      H.liftAff
        $ try do
            DocumentReference.get_ documentRef
    case result of
      Left error -> pure $ Left $ showError error
      Right snapshot -> do
        maybeData <- liftEffect $ DocumentSnapshot._data_ snapshot
        case maybeData of
          Nothing -> pure $ Right Nothing
          Just dat -> pure $ lmap show $ decodeJson dat
