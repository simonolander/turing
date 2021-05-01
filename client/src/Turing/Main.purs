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
import Effect.Console (errorShow, logShow)
import Data.Either (Either(..))

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody

    let
        environment :: Env
        environment = { logLevel: Dev }

        rootComponent :: H.Component Router.Query Router.Input Router.Output Aff
        rootComponent = H.hoist (runAppM environment) Router.component

    void $ H.liftEffect $ onAuthStateChanged $ case _ of
        Left error -> errorShow error
        Right maybeUser -> logShow maybeUser

    halogenIO <- runUI rootComponent unit body
    void $ liftEffect $ matchesWith (parse Route.route) \old new ->
        void $ when (old /= Just new) do
            launchAff_ $ halogenIO.query $ H.mkTell $ Router.Navigate new
