module Turing.Firebase.Auth where

import Prelude
import Control.Monad.Except (runExcept)
import Effect (Effect)
import Effect.Aff (Aff, never, bracket)
import Foreign (Foreign, F, readString, isNull, readBoolean)
import Foreign.Index ((!))
import Control.Promise (Promise)
import Turing.Data.User (User)
import Turing.Data.UserCredential (UserCredential)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Subscription as HS
import Data.Either

foreign import data Auth :: Type
foreign import authImpl :: Effect Auth

foreign import signInAnonymouslyImpl :: Auth -> Effect (Promise Foreign)

signInAnonymously :: Aff UserCredential
signInAnonymously = never

foreign import onAuthStateChangedImpl :: forall a. Auth -> (Foreign -> a) -> Effect (Effect Unit)

--onAuthStateChangedEmitter :: forall m a. MonadAff m => (F (Maybe User) -> a) -> m (HS.Emitter a)
--onAuthStateChangedEmitter value = do
--    { emitter, listener } <- H.liftEffect HS.create
--    _ <- H.liftAff $ H.forkAff do
--    pure emitter

onAuthStateChanged :: (Either _ (Maybe User) -> Effect Unit) -> Effect (Effect Unit)
onAuthStateChanged callback = do
    auth <- authImpl
    onAuthStateChangedImpl auth callback'
    where
    callback' :: Foreign -> Effect Unit
    callback' foreignValue = callback $ runExcept (readMaybeUser foreignValue)

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

