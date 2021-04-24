module Turing.Data.Firebase where

import Data.Maybe (Maybe)

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
type AdditionalUserInfo =
    { isNewUser :: Boolean
    , username :: Maybe String
    }
