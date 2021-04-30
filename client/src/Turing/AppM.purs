module Turing.AppM where

import Prelude
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, runReaderT)
import Control.Parallel (class Parallel, parallel, sequential)
import Effect.Aff (Aff, ParAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Turing.Capability.Navigate (class Navigate)
import Turing.Capability.ManageSpec (class ManageSpec)
import Turing.Data.Env (Env)
import Turing.Data.Route as Route
import Control.Monad.Reader.Class (asks)
import Type.Equality (class TypeEquals, from)
import Routing.Hash (setHash)
import Routing.Duplex (print)
import Data.Maybe (Maybe(..))

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
  getSpec = const $ pure Nothing
