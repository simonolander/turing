module Turing.Component.Form.Program where

import Prelude

import Data.Const (Const)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Turing.Data.Card (Card, CardId)
import Turing.Data.Program (Program)
import Turing.Data.Spec (Spec)
import Type.Proxy (Proxy(..))

newtype ProgramForm (r :: Row Type -> Type) f = ProgramForm (r
    ( id :: f Void String String
    , specId :: f Void String String
    , name :: f NameError String String
    , deck :: f Void String (Map CardId Card)
    ))
derive instance newtypeProgramForm :: Newtype (ProgramForm r f) _

data MaxNumberOfCardsError
    = InvalidInt
    | TooLow

data NameError = EmptyName

type Input =
    { program :: Program
    , spec :: Spec
    }

type Slot = H.Slot (F.Query ProgramForm (Const Void) ()) Program

component :: forall query m.
    MonadEffect m =>
    MonadAff m =>
    F.Component ProgramForm query () Input Program m
component =
    F.component mkInput $ F.defaultSpec
        { render = render
        , handleEvent = F.raiseResult
        }
    where
    mkInput :: Input -> _
    mkInput input =
        { validators: ProgramForm
            { id: F.hoistFn_ $ const input.program.id
            , specId: F.hoistFn_ $ const input.spec.id
            , name: F.hoistFnE_ \str ->
                case String.trim str of
                    "" -> Left EmptyName
                    trimmedName -> Right trimmedName
            , deck: F.hoistFn_ $ const Map.empty
            }
        , initialInputs: Just $ F.wrapInputFields
            { id: input.program.id
            , specId: input.spec.id
            , name: input.program.name
            , deck: ""
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
            , HH.section_
                [ HH.h2_ [ HH.text "Cards" ]
                , HH.button
                    []
                    [ HH.text "Add card" ]
                , HH.section_
                    [ HH.h3_ [ HH.text "Card f99asc0" ]
                    , HH.label_
                        [ HH.text "Id"
                        , HH.input
                            []
                        ]
                    , HH.section_
                        [ HH.h3_ [ HH.text "When on" ]
                        , HH.label_
                            [ HH.text "Write"
                            , HH.input
                                [ HP.type_ HP.InputCheckbox ]
                            ]
                        , HH.label_
                            [ HH.text "Move"
                            , HH.input
                                [ HP.type_ HP.InputCheckbox ]
                            ]
                        , HH.label_
                            [ HH.text "Next card"
                            , HH.input
                                []
                            ]
                        ]
                    ]
                ]
            , HH.button
                [ HE.onClick $ const F.submit ]
                [ HH.text "Save program" ]
            ]
        where
        _name = (Proxy :: Proxy "name")
