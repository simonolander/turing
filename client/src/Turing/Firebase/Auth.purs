module Turing.Firebase.Auth where

import Prelude
import Control.Monad.Except (runExcept)
import Effect (Effect)
import Foreign (Foreign, F, readString, isNull, readBoolean, MultipleErrors)
import Foreign.Index ((!))
import Control.Promise (Promise)
import Turing.Data.User (User)
import Data.Maybe (Maybe(..))
import Data.Either (Either)

foreign import data Auth :: Type
foreign import authImpl :: Effect Auth

foreign import signInAnonymouslyImpl :: Auth -> Effect (Promise Foreign)

foreign import onAuthStateChangedImpl :: forall a. Auth -> (Foreign -> a) -> Effect (Effect Unit)

--onAuthStateChangedEmitter :: forall m a. MonadAff m => (F (Maybe User) -> a) -> m (HS.Emitter a)
--onAuthStateChangedEmitter value = do
--    { emitter, listener } <- H.liftEffect HS.create
--    _ <- H.liftAff $ H.forkAff do
--    pure emitter

onAuthStateChanged :: forall a. (Either MultipleErrors (Maybe User) -> a) -> Effect (Effect Unit)
onAuthStateChanged callback = do
    auth <- authImpl
    onAuthStateChangedImpl auth callback'
    where
    callback' :: Foreign -> a
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

