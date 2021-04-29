module Turing.Component.Form.SpecP where

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

type Contact = { name :: String, text :: String }

newtype ContactForm (r :: Row Type -> Type) f = ContactForm (r
  ( name :: f Void String String
  , text :: f Void String String
  ))
derive instance newtypeContactForm :: Newtype (ContactForm r f) _

data Action = HandleContact Contact

component :: forall m.
    MonadEffect m =>
    MonadAff m =>
    H.Component (Const Void) Unit Void m
component = H.mkComponent
  { initialState: const unit
  , render: const render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction = case _ of
    HandleContact contact -> H.liftEffect $ logShow (contact :: Contact)

  render =
    HH.section_
      [ HH.h1_ [ HH.text "Formless" ]
      , HH.h2_ [ HH.text "A basic contact form." ]
      , HH.text
          """
          You can create a full Halogen contact form like this in less than 20 lines of Formless, excluding the render function.  It's type-safe, supports complex types, has validation, and parses to the output type of your choice."
          """
      , HH.br_
      , HH.slot F._formless unit formComponent unit HandleContact
      ]

  formComponent :: F.Component ContactForm (Const Void) () Unit Contact m
  formComponent = F.component (const formInput) $ F.defaultSpec { render = renderFormless, handleEvent = F.raiseResult }
    where
    formInput =
      { validators: ContactForm { name: F.noValidation, text: F.noValidation }
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
     _text = Proxy :: Proxy "text"
