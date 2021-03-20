module Main where

import Prelude

import App.Button as Button
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Turing.Env

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody

    let
        logLevel = Debug
        baseUrl = "https://turing.simonolander.org"

    let
        environment :: Env
        environment = { logLevel, baseUrl }

--    rootComponent :: H.Component

    runUI Button.component unit body
