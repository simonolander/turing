module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Turing.Component.Router as Router
import Turing.Data.Env (Env, LogLevel(..))
import Turing.Data.Route as Route
import Turing.AppM (runAppM)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Turing.Firebase.Auth (onAuthStateChanged)
import Effect.Console (errorShow)
import Data.Either (Either(..))
import Network.RemoteData (RemoteData(..))
import Effect.Ref as Ref

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody

    userRef <- liftEffect do
        ref <- Ref.new Loading

        void $ onAuthStateChanged $ case _ of
            Left error -> do
                errorShow error
                Ref.write (Failure error) ref
            Right maybeUser -> do
--                logShow maybeUser
                Ref.write (Success maybeUser) ref
        pure ref

    let
        environment :: Env
        environment =
            { logLevel: Dev
            , userRef
            }

        rootComponent :: H.Component Router.Query Router.Input Router.Output Aff
        rootComponent = H.hoist (runAppM environment) Router.component

    halogenIO <- runUI rootComponent unit body
    void $ liftEffect $ matchesWith (parse Route.route) \old new ->
        void $ when (old /= Just new) do
            launchAff_ $ halogenIO.query $ H.mkTell $ Router.Navigate new
