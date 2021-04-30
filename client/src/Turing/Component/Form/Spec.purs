module Turing.Component.Form.Spec where

import Prelude
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Class (class MonadEffect)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Turing.Data.Spec (Spec)
import Data.Int as Int

newtype SpecForm (r :: Row Type -> Type) f = SpecForm (r
    ( id :: f Void String String
    , name :: f Void String String
    , maxNumberOfCards :: f Void String Int
    ))
derive instance newtypeSpecForm :: Newtype (SpecForm r f) _

type Query :: forall k. k -> Type
type Query = Const Void

type Input = String

type Slot = H.Slot (F.Query SpecForm Query ()) Spec

component :: forall m.
    MonadEffect m =>
    MonadAff m =>
    F.Component SpecForm Query () Input Spec m
component =
    F.component mkInput $ F.defaultSpec
        { render = render
        , handleEvent = F.raiseResult
        }
    where
    mkInput :: String -> _
    mkInput specId =
        { validators: SpecForm
            { id: F.hoistFn_ $ const specId
            , name: F.noValidation
            , maxNumberOfCards: F.hoistFnE_ \str ->
                case Int.fromString str of
                    Nothing -> Right 0
                    Just n -> Right n
            }
        , initialInputs: Nothing
        }

    render { form } =
        HH.div_
            [ HH.p_
                [ HH.label_
                    [ HH.text "Name"
                    , HH.input
                        [ HP.value $ F.getInput _name form
                        , HE.onValueInput $ F.setValidate _name
                        ]
                    ]
                ]
            , HH.p_
                [ HH.label_
                    [ HH.text "Maximum number of cards"
                    , HH.input
                        [ HP.type_ HP.InputNumber
                        , HP.min 0.0
                        , HP.value $ F.getInput _maxNumberOfCards form
                        , HE.onValueInput $ F.setValidate _maxNumberOfCards
                        ]
                    ]
                ]
            , HH.button
                [ HE.onClick $ const F.submit ]
                [ HH.text "Submit" ]
            ]
        where
        _name = (Proxy :: Proxy "name")
        _maxNumberOfCards = Proxy :: Proxy "maxNumberOfCards"


