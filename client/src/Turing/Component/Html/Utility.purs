module Turing.Component.Html.Utility where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Turing.Data.Route (Route, route)
import Routing.Duplex (print)
import Network.RemoteData (RemoteData(..))
import Data.Maybe (Maybe)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Turing.Data.Route (Route(..))

safeHref :: forall r i. Route -> HH.IProp ( href :: String | r) i
safeHref = HP.href <<< append "#" <<< print route

whenFailure :: forall e a w i. RemoteData e a -> (e -> HH.HTML w i) -> HH.HTML w i
whenFailure (Failure e) f = f e
whenFailure _ _ = HH.text ""

notAsked :: forall w i. HH.HTML w i
notAsked =
    HH.div_
        [ HH.h1_ [ HH.text "Resource not requested" ]
        , HH.p_
            [ HH.text
                """
                The resource that this page is supposed to display has not been asked for.
                This is usually an error.
                """
            ]
        ]

loading :: forall w i. String -> String -> HH.HTML w i
loading resourceType id =
    HH.div_
        [ HH.h1_ [ HH.text $ "Loading " <> resourceType ]
        , HH.p_ [ HH.text $ "The " <> resourceType <> " " <> show id <> " is loading, and will be available shortly." ]
        ]

failure :: forall w i. String -> HH.HTML w i
failure error =
    HH.div_
        [ HH.h1_ [ HH.text "Error" ]
        , HH.p_ [ HH.text "There was an error performing the task at hand. See the error message for details." ]
        , HH.p_ [ HH.text error ]
        ]

notFound :: forall w i. String -> String -> HH.HTML w i
notFound resourceType id =
    HH.div_
        [ HH.h1_ [ HH.text "404 - Not found" ]
        , HH.p_ [ HH.text $ "The " <> resourceType <> " " <> id <> " could not be found." ]
        ]

remoteData :: forall a w i. RemoteData String (Maybe a) -> String -> String -> (a -> HH.HTML w i) -> HH.HTML w i
remoteData rd resourceType id f =
    case rd of
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
