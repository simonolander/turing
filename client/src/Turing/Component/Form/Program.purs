module Turing.Component.Form.Program where

import Prelude

import Data.Array ((:))
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Turing.Component.Form.Card as CardForm
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

type ChildSlots =
    ( cardForm :: CardForm.Slot Int )

type Slot = H.Slot (F.Query ProgramForm (Const Void) ChildSlots) Program

data Action
    = ClickedNewCard
    | HandleCardForm Int CardForm.Message

component :: forall query m.
    MonadEffect m =>
    MonadAff m =>
    F.Component ProgramForm query ChildSlots Input Program m
component =
    F.component mkInput $ F.defaultSpec
        { render = render
        , handleEvent = F.raiseResult
        , handleAction = handleAction
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
        , formIds: []
        , nextId: 0
        }

    render state =
        HH.div_
            [ HH.p_
                [ HH.label_
                    [ HH.text "Name"
                    , HH.input
                        [ HP.value $ F.getInput _name state.form
                        , HE.onValueInput $ F.setValidate _name
                        ]
                    , HH.text
                        case F.getError _name state.form of
                            Just EmptyName -> "Name cannot be empty"
                            Nothing -> ""
                    ]
                ]
            , HH.section_
                [ HH.h2_ [ HH.text "Cards" ]
                , HH.button
                    [ HE.onClick $ const $ F.injAction ClickedNewCard ]
                    [ HH.text "Add card" ]
                , if state.formIds == [] then
                    HH.p_ [ HH.text "No cards" ]
                  else
                    HH.div_ $ mkCardForm <$> state.formIds
                ]
            , HH.button
                [ HE.onClick $ const F.submit ]
                [ HH.text "Save program" ]
            ]
        where
        _name = (Proxy :: Proxy "name")
        _cardForm = (Proxy :: Proxy "cardForm")

        mkCardForm id = do
            let
                handler = F.injAction <<< HandleCardForm id

                card :: Card
                card =
                    { id: show id
                    , instructions: Tuple
                        { writeSymbol: false
                        , tapeMotion: false
                        , nextCardId: Just (show id)
                        }
                        { writeSymbol: true
                        , tapeMotion: true
                        , nextCardId: Just (show id)
                        }
                    }
            HH.slot _cardForm id CardForm.component card handler

    handleAction action =
        case action of
            ClickedNewCard -> H.modify_ \state -> state
                { formIds = state.nextId : state.formIds
                , nextId = state.nextId + 1
                }
            HandleCardForm id CardForm.Remove -> pure unit
