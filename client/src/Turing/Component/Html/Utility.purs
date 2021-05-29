module Turing.Component.Html.Utility where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Routing.Duplex (print)
import Turing.Data.Route (Route(..), route)
import Data.Array as Array

safeHref :: forall r i. Route -> HH.IProp ( href :: String | r ) i
safeHref = HP.href <<< append "#" <<< print route

whenFailure :: forall e a w i. RemoteData e a -> (e -> HH.HTML w i) -> HH.HTML w i
whenFailure (Failure e) f = f e

whenFailure _ _ = HH.text ""

classes :: forall r i. String -> HH.IProp ( class :: String | r ) i
classes cls = HP.classes $ wrap <$> String.split (wrap " ") cls

notAsked :: forall w i. HH.HTML w i
notAsked =
  HH.div
    [ classes "block" ]
    [ HH.div
        [ classes "notification is-warning is-light" ]
        [ HH.h1
            [ classes "title is-6" ]
            [ HH.text "Resource not requested" ]
        , HH.p_
            [ HH.text "The resource that this page is supposed to display has not been asked for. This is usually an error." ]
        ]
    ]

loading :: forall w i. String -> String -> HH.HTML w i
loading resourceType id =
  HH.div
    [ classes "block" ]
    [ HH.progress [ classes "progress is-primary" ] []
    , HH.p_ [ HH.text $ "The " <> resourceType <> " " <> id <> " is loading, and will be available shortly." ]
    ]

failure :: forall w i. String -> HH.HTML w i
failure error =
  HH.div
    [ classes "block" ]
    [ HH.div
        [ classes "notification is-danger is-light" ]
        [ HH.h1
            [ classes "title is-6" ]
            [ HH.text "Error" ]
        , HH.p
            [ classes "block" ]
            [ HH.text "There was an error performing the task at hand." ]
        , HH.p
            [ classes "block" ]
            [ HH.text error ]
        ]
    ]

notFound :: forall w i. String -> String -> HH.HTML w i
notFound resourceType id =
  HH.div
    [ classes "block" ]
    [ HH.div
        [ classes "notification is-info is-light" ]
        [ HH.h1
            [ classes "title is-6" ]
            [ HH.text "404 - Not found" ]
        , HH.p
            [ classes "block" ]
            [ HH.text $ "The " <> resourceType <> " " <> id <> " could not be found." ]
        ]
    ]

remoteData :: forall a w i. RemoteData String (Maybe a) -> String -> String -> (a -> HH.HTML w i) -> HH.HTML w i
remoteData rd resourceType id f = case rd of
  NotAsked -> notAsked
  Loading -> loading resourceType id
  Failure error -> failure error
  Success Nothing -> notFound resourceType id
  Success (Just value) -> f value

navbar :: forall w i. HH.HTML w i
navbar =
  HH.nav
    [ HP.class_ $ wrap "navbar" ]
    [ HH.div
        [ HP.class_ $ wrap "navbar-brand" ]
        [ HH.a
            [ safeHref Home
            , HP.class_ $ wrap "navbar-item"
            ]
            [ HH.img
                [ HP.src "https://freepngimg.com/thumb/light/78155-icons-light-idea-computer-lighting-incandescent-bulb.png"
                , HP.class_ $ wrap "image"
                ]
            ]
        , HH.a
            [ safeHref Home
            , HP.class_ $ wrap "navbar-item"
            ]
            [ HH.h1
                [ HP.classes $ wrap <$> [ "title", "is-4" ] ]
                [ HH.text "Turing" ]
            ]
        ]
    , HH.div
        [ HP.class_ $ wrap "navbar-menu" ]
        [ HH.div
            [ HP.class_ $ wrap "navbar-end" ]
            [ HH.a
                [ safeHref Home
                , HP.class_ $ wrap "navbar-item"
                ]
                [ HH.text "Home" ]
            ]
        ]
    ]

section_ :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
section_ elements =
  HH.section
    [ HP.class_ $ wrap "section" ]
    elements

container_ :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
container_ elements =
  HH.div
    [ HP.class_ $ wrap "container" ]
    elements

title_ :: forall w i. String -> HH.HTML w i
title_ text =
  HH.h1
    [ HP.classes $ wrap <$> [ "title", "is-2" ] ]
    [ HH.text text ]

title2_ :: forall w i. String -> HH.HTML w i
title2_ text =
  HH.h2
    [ HP.classes $ wrap <$> [ "title", "is-3" ] ]
    [ HH.text text ]
