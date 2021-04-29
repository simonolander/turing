module Turing.Component.Form.Spec where

import Prelude
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Class (class MonadEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Turing.Data.Spec (Spec)
import Data.Int as Int

newtype SpecForm (r :: Row Type -> Type) f = SpecForm (r
    ( name :: f Void String String
    , maxNumberOfCards :: f Void String Int
    ))
derive instance newtypeSpecForm :: Newtype (SpecForm r f) _

type Slot = H.Slot (F.Query SpecForm (Const Void) ()) Spec

--data Action = HandleSpec Spec

component :: forall m.
    MonadEffect m =>
    MonadAff m =>
    F.Component SpecForm (Const Void) () Unit Spec m
component = F.component (const formInput) $ F.defaultSpec { render = renderFormless, handleEvent = F.raiseResult }
    where
    formInput =
        { validators: SpecForm
            { name: F.noValidation
            , maxNumberOfCards: F.hoistFnE_ \str ->
                case Int.fromString str of
                    Nothing -> Right 0
                    Just n -> Right n
            }
        , initialInputs: Nothing
        }

    renderFormless st =
        HH.div_
            [ HH.input []
            , HH.textarea []
            , HH.button
                [ HE.onClick \_ -> F.submit ]
                [ HH.text "Submit" ]
            ]
        where
        _name = Proxy :: Proxy "name"
        _text = Proxy :: Proxy "maxNumberOfCards"

--component :: forall m.
--  MonadEffect m =>
--  MonadAff m =>
--  H.Component (Const Void) Unit Void m
--component = H.mkComponent
--  { initialState: const unit
--  , render: const render
--  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
--  }
--  where
--  handleAction = case _ of
--  HandleSpec contact -> H.liftEffect $ logShow (contact :: Spec)
--
--  render =forall m.
      ----  MonadEffect m =>
      ----  MonadAff m =>
--  HH.section_
--    [ HH.h1_ [ HH.text "Formless" ]
--    , HH.h2_ [ HH.text "A basic contact form." ]
--    , HH.text
--      """
--      You can create a full Halogen contact form like this in less than 20 lines of Formless, excluding the render function.  It's type-safe, supports complex types, has validation, and parses to the output type of your choice."
--      """
--    , HH.br_
--    , HH.slot F._formless unit formComponent unit HandleSpec
--    ]


