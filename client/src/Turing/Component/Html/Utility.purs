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

safeHref :: forall r i. Route -> HH.IProp ( href :: String | r) i
safeHref = HP.href <<< append "#" <<< print route

whenFailure :: forall e a w i. RemoteData e a -> (e -> HH.HTML w i) -> HH.HTML w i
whenFailure (Failure e) f = f e
whenFailure _ _ = HH.text ""

notAsked :: forall slots action m. HH.HTML (H.ComponentSlot slots m action) action
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

loading :: forall slots action m. String -> String -> HH.HTML (H.ComponentSlot slots m action) action
loading resourceType id =
    HH.div_
        [ HH.h1_ [ HH.text $ "Loading " <> resourceType ]
        , HH.p_ [ HH.text $ "The " <> resourceType <> " " <> show id <> " is loading, and will be available shortly." ]
        ]

failure :: forall slots action m. String -> HH.HTML (H.ComponentSlot slots m action) action
failure error =
    HH.div_
        [ HH.h1_ [ HH.text "Error" ]
        , HH.p_ [ HH.text "There was an error performing the task at hand. See the error message for details." ]
        , HH.p_ [ HH.text error ]
        ]

notFound :: forall slots action m. String -> String -> HH.HTML (H.ComponentSlot slots m action) action
notFound resourceType id =
    HH.div_
        [ HH.h1_ [ HH.text "404 - Not found" ]
        , HH.p_ [ HH.text $ "The " <> resourceType <> " " <> id <> " could not be found." ]
        ]

remoteData :: forall a slots action m. RemoteData String (Maybe a) -> String -> String -> (a -> HH.HTML (H.ComponentSlot slots m action) action) -> HH.HTML (H.ComponentSlot slots m action) action
remoteData rd resourceType id f =
    case rd of
        NotAsked -> notAsked
        Loading -> loading resourceType id
        Failure error -> failure error
        Success Nothing -> notFound resourceType id
        Success (Just value) -> f value
