module Turing.Firebase.CollectionReference where

import Prelude
import Effect (Effect)
import Control.Promise (Promise)

foreign import data CollectionReference :: Type -> Type
foreign import data DocumentData :: Type
foreign import data DocumentReference :: Type -> Type

type DocumentPath = String

foreign import doc :: forall t. CollectionReference t -> DocumentPath -> Effect (DocumentReference t)
