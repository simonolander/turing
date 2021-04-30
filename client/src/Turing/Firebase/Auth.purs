module Turing.Firebase.Auth where

import Prelude
import Control.Monad.Except (runExcept)
import Effect (Effect)
import Effect.Aff (Aff, never)
import Foreign (Foreign, F, readString, isNull, readBoolean)
import Foreign.Index ((!))
import Control.Promise (Promise)
import Turing.Data.User (User)
import Turing.Data.UserCredential (UserCredential)
import Data.Maybe (Maybe(..))

foreign import data Auth :: Type
foreign import authImpl :: Effect Auth

foreign import signInAnonymouslyImpl :: Auth -> Effect (Promise Foreign)

signInAnonymously :: Aff UserCredential
signInAnonymously = never

foreign import onAuthStateChangedImpl :: forall a. Auth -> (Foreign -> a) -> Effect Unit

onAuthStateChanged :: (F (Maybe User) -> Effect Unit) -> Effect Unit
onAuthStateChanged nextOrObserver = do
    auth <- authImpl
    onAuthStateChangedImpl auth (readMaybeUser >>> nextOrObserver)

readMaybeUser :: Foreign -> F (Maybe User)
readMaybeUser value
    | isNull value = pure Nothing
    | otherwise = Just <$> readUser value

readUser :: Foreign -> F User
readUser value = do
    uid <- value ! "uid" >>= readString
    displayName <- readDisplayName value
    isAnonymous <- value ! "isAnonymous" >>= readBoolean
    pure { uid, displayName, isAnonymous }
    where
    readDisplayName :: Foreign -> F (Maybe String)
    readDisplayName _ = pure Nothing

