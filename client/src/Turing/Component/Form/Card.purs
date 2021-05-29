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
  = Card

type Slot
  = H.Slot (F.Query CardForm (Const Void) ()) Message

data Action
  = ClickedRemove

data Message
  = Remove

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
        }
  where
  mkInput :: Input -> _
  mkInput card =
    { validators:
        CardForm
          { id: F.hoistFn_ $ const card.id
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
              { id: card.id
              , falseWriteSymbol: show $ _.writeSymbol $ fst card.instructions
              , falseTapeMotion: show $ _.tapeMotion $ fst card.instructions
              , falseNextCardId: show $ _.nextCardId $ fst card.instructions
              , trueWriteSymbol: show $ _.writeSymbol $ snd card.instructions
              , trueTapeMotion: show $ _.tapeMotion $ snd card.instructions
              , trueNextCardId: show $ _.nextCardId $ snd card.instructions
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
              , HH.text case F.getError _name form of
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
    _name = (Proxy :: Proxy "id")

  handleAction ClickedRemove = H.raise Remove
