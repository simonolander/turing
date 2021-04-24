module Turing.Data.Firebase where

import Prelude
import Data.Maybe (Maybe)
import Data.Argonaut.Decode (class DecodeJson, (.:), (.:?), decodeJson)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Generic.Rep.Show (genericShow)

--| See https://firebase.google.com/docs/reference/js/firebase.auth#usercredential
type UserCredential =
    { additionalUserInfo :: Maybe AdditionalUserInfo
    , user :: Maybe User
    }

--| See https://firebase.google.com/docs/reference/js/firebase.User
type User =
    { displayName :: Maybe String
    , email :: Maybe String
    , emailVerified :: Boolean
    , isAnonymous :: Boolean
    , photoURL :: Maybe String
    , providerId :: String
    , refreshToken :: String
    , tenantId :: Maybe String
    , uid :: String
    }

--| See https://firebase.google.com/docs/reference/js/firebase.auth#additionaluserinfo
newtype AdditionalUserInfo = AdditionalUserInfo
    { isNewUser :: Boolean
    , username :: Maybe String
    }

derive instance newtypeAdditionalUserInfo :: Newtype AdditionalUserInfo _
derive instance genericAdditionalUserInfo :: Generic AdditionalUserInfo _

instance showAdditionalUserInfo :: Show AdditionalUserInfo where
    show = genericShow

instance decodeJsonAdditionalUserInfo :: DecodeJson AdditionalUserInfo where
    decodeJson json = do
        decoded <- decodeJson json
        isNewUser <- decoded .: "isNewUser"
        username <- decoded .:? "username"
        pure $ AdditionalUserInfo { isNewUser, username }
