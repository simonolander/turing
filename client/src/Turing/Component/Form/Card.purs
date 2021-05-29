module Turing.Component.Form.Card where

import Prelude
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Turing.Data.Card (Card, CardId)
import Turing.Data.Spec (Spec)
import Type.Proxy (Proxy(..))
import Turing.Component.Html.Utility (classes)
import Data.Maybe (isJust)
import Data.Array ((:))
import Effect.Class.Console (logShow)
import Data.Tuple (Tuple)
import Data.Maybe (fromMaybe)

newtype CardForm (r :: Row Type -> Type) f
  = CardForm
  ( r
      ( id :: f NameError String String
      , falseWriteSymbol :: f Void String Boolean
      , falseTapeMotion :: f Void String Boolean
      , falseNextCardId :: f Void String (Maybe CardId)
      , trueWriteSymbol :: f Void String Boolean
      , trueTapeMotion :: f Void String Boolean
      , trueNextCardId :: f Void String (Maybe CardId)
      )
  )

derive instance newtypeCardForm :: Newtype (CardForm r f) _

data MaxNumberOfCardsError
  = InvalidInt
  | TooLow

data NameError
  = EmptyName

type Input
  = { card :: Card
    , cardNames :: Array (Tuple Int String)
    }

type State
  = ( cardNames :: Array (Tuple Int String) )

type Slot
  = H.Slot (F.Query CardForm (Const Void) ()) Message

data Action
  = ClickedRemove
  | ChangedName String
  | Receive Input

data Message
  = Remove
  | Name String

component ::
  forall query m.
  MonadEffect m =>
  MonadAff m =>
  F.Component CardForm query () Input Message m
component =
  F.component mkInput
    $ F.defaultSpec
        { render = render
        , handleAction = handleAction
        , handleEvent = handleEvent
        , receive = Just <<< Receive
        }
  where
  mkInput :: Input -> F.Input CardForm State m
  mkInput input =
    { validators:
        CardForm
          { id:
              F.hoistFnE_ \str -> case String.trim str of
                "" -> Left EmptyName
                trimmedName -> Right trimmedName
          , falseWriteSymbol: F.hoistFn_ $ const false
          , falseTapeMotion: F.hoistFn_ $ const false
          , falseNextCardId: F.hoistFn_ $ const Nothing
          , trueWriteSymbol: F.hoistFn_ $ const false
          , trueTapeMotion: F.hoistFn_ $ const false
          , trueNextCardId: F.hoistFn_ $ const Nothing
          }
    , initialInputs:
        Just
          $ F.wrapInputFields
              { id: input.card.id
              , falseWriteSymbol: input.card.instructions # fst # _.writeSymbol # show
              , falseTapeMotion: input.card.instructions # fst # _.tapeMotion # show
              , falseNextCardId: input.card.instructions # fst # _.nextCardId # fromMaybe ""
              , trueWriteSymbol: input.card.instructions # snd # _.writeSymbol # show
              , trueTapeMotion: input.card.instructions # snd # _.tapeMotion # show
              , trueNextCardId: input.card.instructions # snd # _.nextCardId # fromMaybe ""
              }
    , cardNames: input.cardNames
    }

  render state =
    let
      name = F.getInput _name state.form

      nameError = F.getError _name state.form
    in
      HH.div
        [ classes "column is-narrow" ]
        [ HH.div
            [ classes "card" ]
            [ HH.div
                [ classes "card-header" ]
                [ HH.p
                    [ classes "card-header-title" ]
                    [ HH.text $ "Card " <> name ]
                , HH.div
                    [ classes "card-header-icon"
                    ]
                    [ HH.button
                        [ HE.onClick $ const $ F.injAction ClickedRemove
                        , classes "delete"
                        ]
                        []
                    ]
                ]
            , HH.div
                [ classes "card-content" ]
                [ HH.div
                    [ classes "field block" ]
                    [ HH.label
                        [ classes "label" ]
                        [ HH.text "Name" ]
                    , HH.div
                        [ classes "control" ]
                        [ HH.input
                            [ classes $ if isJust nameError then "input is-danger" else "input"
                            , HP.type_ HP.InputText
                            , HP.placeholder "My card"
                            , HP.value name
                            , HE.onValueInput $ F.injAction <<< ChangedName
                            ]
                        ]
                    , case nameError of
                        Just EmptyName ->
                          HH.p
                            [ classes "help is-danger" ]
                            [ HH.text "Cannot be empty" ]
                        Nothing -> HH.text ""
                    ]
                , HH.div
                    [ classes "columns" ]
                    [ HH.div
                        [ classes "column" ]
                        [ HH.h1
                            [ classes "title is-5" ]
                            [ HH.text "When on" ]
                        , HH.div
                            [ classes "field" ]
                            [ HH.label
                                [ classes "checkbox" ]
                                [ HH.input
                                    [ HP.type_ HP.InputCheckbox
                                    , HP.value $ F.getInput _trueWriteSymbol state.form
                                    , HE.onValueInput $ F.setValidate _trueWriteSymbol
                                    ]
                                , HH.text " Write"
                                ]
                            ]
                        , HH.div
                            [ classes "field" ]
                            [ HH.label
                                [ classes "checkbox" ]
                                [ HH.input
                                    [ HP.type_ HP.InputCheckbox
                                    , HP.value $ F.getInput _trueWriteSymbol state.form
                                    , HE.onValueInput $ F.setValidate _trueTapeMotion
                                    ]
                                , HH.text " Move left"
                                ]
                            ]
                        , HH.div
                            [ classes "field" ]
                            [ HH.label
                                [ classes "label" ]
                                [ HH.text "Next card" ]
                            , HH.div
                                [ classes "select" ]
                                [ HH.select
                                    [ HE.onValueChange $ F.setValidate _trueNextCardId ]
                                    $ (HH.option [ HP.value "", classes "is-italic" ] [ HH.text "<Terminate>" ])
                                    : map (\c -> HH.option_ [ HH.text c ]) (state.cardNames <#> snd)
                                ]
                            ]
                        ]
                    , HH.div
                        [ classes "column" ]
                        [ HH.h1
                            [ classes "title is-5" ]
                            [ HH.text "When off" ]
                        , HH.div
                            [ classes "field" ]
                            [ HH.label
                                [ classes "checkbox" ]
                                [ HH.input
                                    [ HP.type_ HP.InputCheckbox
                                    , HP.value $ F.getInput _falseWriteSymbol state.form
                                    , HE.onValueInput $ F.setValidate _falseWriteSymbol
                                    ]
                                , HH.text " Write"
                                ]
                            ]
                        , HH.div
                            [ classes "field" ]
                            [ HH.label
                                [ classes "checkbox" ]
                                [ HH.input
                                    [ HP.type_ HP.InputCheckbox
                                    , HP.value $ F.getInput _falseWriteSymbol state.form
                                    , HE.onValueInput $ F.setValidate _falseTapeMotion
                                    ]
                                , HH.text " Move left"
                                ]
                            ]
                        , HH.div
                            [ classes "field" ]
                            [ HH.label
                                [ classes "label" ]
                                [ HH.text "Next card" ]
                            , HH.div
                                [ classes "select" ]
                                [ HH.select
                                    [ HE.onValueChange $ F.setValidate _falseNextCardId ]
                                    $ (HH.option [ HP.value "", classes "is-italic" ] [ HH.text "<Terminate>" ])
                                    : map (\c -> HH.option_ [ HH.text c ]) (state.cardNames <#> snd)
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    where
    _name = (Proxy :: Proxy "id")

    _falseWriteSymbol = (Proxy :: Proxy "falseWriteSymbol")

    _falseTapeMotion = (Proxy :: Proxy "falseTapeMotion")

    _falseNextCardId = (Proxy :: Proxy "falseNextCardId")

    _trueWriteSymbol = (Proxy :: Proxy "trueWriteSymbol")

    _trueTapeMotion = (Proxy :: Proxy "trueTapeMotion")

    _trueNextCardId = (Proxy :: Proxy "trueNextCardId")

  handleEvent = const $ pure unit

  handleAction = case _ of
    ClickedRemove -> H.raise Remove
    ChangedName name -> do
      H.raise $ Name name
      F.handleAction handleAction handleEvent $ F.setValidate (Proxy :: Proxy "id") name
    Receive input -> do
      H.modify_ _ { cardNames = input.cardNames }
