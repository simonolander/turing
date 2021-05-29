module Turing.Component.Form.Program where

import Prelude
import Data.Array (snoc, (:))
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Turing.Component.Form.Card as CardForm
import Turing.Component.Html.Utility (classes, section_)
import Turing.Data.Card (Card, CardId)
import Turing.Data.Program (Program)
import Turing.Data.Spec (Spec)
import Type.Proxy (Proxy(..))
import Data.Tuple (fst)
import Data.Array (findIndex)
import Data.Array (modifyAt)
import Data.Bifunctor (rmap)

newtype ProgramForm (r :: Row Type -> Type) f
  = ProgramForm
  ( r
      ( id :: f Void String String
      , specId :: f Void String String
      , name :: f NameError String String
      , deck :: f Void String (Map CardId Card)
      )
  )

derive instance newtypeProgramForm :: Newtype (ProgramForm r f) _

data MaxNumberOfCardsError
  = InvalidInt
  | TooLow

data NameError
  = EmptyName

type Input
  = { program :: Program
    , spec :: Spec
    }

type ChildSlots
  = ( cardForm :: CardForm.Slot Int )

type Slot
  = H.Slot (F.Query ProgramForm (Const Void) ChildSlots) Program

data Action
  = ClickedNewCard
  | HandleCardForm Int CardForm.Message

type State
  = ( cardNames :: Array (Tuple Int String)
    , nextCardId :: Int
    )

component ::
  forall query m.
  MonadEffect m =>
  MonadAff m =>
  F.Component ProgramForm query ChildSlots Input Program m
component =
  F.component mkInput
    $ F.defaultSpec
        { render = render
        , handleEvent = F.raiseResult
        , handleAction = handleAction
        }
  where
  mkInput :: Input -> F.Input ProgramForm State m
  mkInput input =
    { validators:
        ProgramForm
          { id: F.hoistFn_ $ const input.program.id
          , specId: F.hoistFn_ $ const input.spec.id
          , name:
              F.hoistFnE_ \str -> case String.trim str of
                "" -> Left EmptyName
                trimmedName -> Right trimmedName
          , deck: F.hoistFn_ $ const Map.empty
          }
    , initialInputs: Nothing
    , cardNames: []
    , nextCardId: 1
    }

  render state =
    let
      nameError = F.getError _name state.form
    in
      HH.div_
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
                    , HP.placeholder "My program"
                    , HP.value $ F.getInput _name state.form
                    , HE.onValueInput $ F.setValidate _name
                    ]
                ]
            , case nameError of
                Just EmptyName ->
                  HH.p
                    [ classes "help is-danger" ]
                    [ HH.text "Cannot be empty" ]
                Nothing -> HH.text ""
            ]
        , HH.section
            [ classes "block" ]
            [ HH.h1
                [ classes "title is-4" ]
                [ HH.text "Cards" ]
            , HH.button
                [ HE.onClick $ const $ F.injAction ClickedNewCard
                , classes "button is-info block"
                ]
                [ HH.text "Add card" ]
            , if state.cardNames == [] then
                HH.div
                  [ classes "notification is-info is-light block" ]
                  [ HH.p_ [ HH.text "You have not added any cards to this program." ] ]
              else
                HH.div
                  [ classes "columns is-multiline" ]
                  $ mkCardForm
                  <$> state.cardNames
            ]
        , HH.button
            [ HE.onClick $ const F.submit
            , classes "button is-primary"
            ]
            [ HH.text "Save program" ]
        ]
    where
    _name = (Proxy :: Proxy "name")

    _cardForm = (Proxy :: Proxy "cardForm")

    mkCardForm :: Tuple Int String -> _
    mkCardForm cardName = do
      let
        id = fst cardName

        handler = F.injAction <<< HandleCardForm id

        card :: Card
        card =
          { id: show id
          , instructions:
              Tuple
                { writeSymbol: false
                , tapeMotion: false
                , nextCardId: Just (show id)
                }
                { writeSymbol: true
                , tapeMotion: true
                , nextCardId: Just (show id)
                }
          }

        input :: CardForm.Input
        input = { card, cardNames: state.cardNames }
      HH.slot _cardForm id CardForm.component input handler

  updateCardNames newCardNames = do
    H.modify_ _ { cardNames = newCardNames }

  handleAction action = case action of
    ClickedNewCard ->
      H.modify_ \state ->
        let
          cardName = Tuple state.nextCardId (show state.nextCardId)
        in
          state
            { cardNames = state.cardNames `snoc` cardName
            , nextCardId = state.nextCardId + 1
            }
    HandleCardForm id CardForm.Remove -> do
      H.modify_ \state ->
        let
          cardNames = Array.filter (fst >>> notEq id) state.cardNames

          nextCardId = cardNames <#> fst # maximum # fromMaybe 0 # add 1
        in
          state
            { cardNames = cardNames
            , nextCardId = nextCardId
            }
    HandleCardForm id (CardForm.Name name) -> do
      cardNames <- H.gets _.cardNames
      let
        cardNames' =
          fromMaybe cardNames do
            index <- findIndex (fst >>> eq id) cardNames
            modifyAt index (rmap $ const name) cardNames
      updateCardNames cardNames'
