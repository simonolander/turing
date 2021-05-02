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
import Data.String as String

newtype SpecForm (r :: Row Type -> Type) f = SpecForm (r
    ( id :: f Void String String
    , name :: f NameError String String
    , maxNumberOfCards :: f MaxNumberOfCardsError String Int
    ))
derive instance newtypeSpecForm :: Newtype (SpecForm r f) _

data MaxNumberOfCardsError
    = InvalidInt
    | TooLow

data NameError = EmptyName

type Query :: forall k. k -> Type
type Query = Const Void

type Input = Spec

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
    mkInput :: Input -> _
    mkInput spec =
        { validators: SpecForm
            { id: F.hoistFn_ $ const spec.id
            , name: F.hoistFnE_ \str ->
                case String.trim str of
                    "" -> Left EmptyName
                    trimmedName -> Right trimmedName
            , maxNumberOfCards: F.hoistFnE_ \str ->
                case Int.fromString str of
                    Nothing -> Left InvalidInt
                    Just n
                        | n < 1 -> Left TooLow
                        | otherwise -> Right n
            }
        , initialInputs: Just $ F.wrapInputFields
            { id: spec.id
            , name: spec.name
            , maxNumberOfCards: show spec.maxNumberOfCards
            }
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
                    , HH.text
                        case F.getError _name form of
                            Just EmptyName -> "Name cannot be empty"
                            Nothing -> ""
                    ]
                ]
            , HH.p_
                [ HH.label_
                    [ HH.text "Maximum number of cards"
                    , HH.input
                        [ HP.type_ HP.InputNumber
                        , HP.min 1.0
                        , HP.value $ F.getInput _maxNumberOfCards form
                        , HE.onValueInput $ F.setValidate _maxNumberOfCards
                        ]
                    , HH.text
                        case F.getError _maxNumberOfCards form of
                            Just InvalidInt -> "Must be a valid integer"
                            Just TooLow -> "Must be greater than zero"
                            Nothing -> ""
                    ]
                ]
            , HH.button
                [ HE.onClick $ const F.submit ]
                [ HH.text "Save spec" ]
            ]
        where
        _name = (Proxy :: Proxy "name")
        _maxNumberOfCards = Proxy :: Proxy "maxNumberOfCards"


