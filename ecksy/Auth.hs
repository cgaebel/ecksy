{-# LANGUAGE OverloadedStrings #-}
module Auth ( isLoggedIn
            , requireLogin
            , checkPassword
            , addPasswordToSession
            , removePasswordFromSession
            ) where

import Import

import Yesod.Default.Config

-- | The key we use to identify a session's password.
sessionKey :: Text
sessionKey = "password"

-- | Requires a login to run the given handler. If the user isn't logged in,
--   they will be redirected to the login screen.
requireLogin :: Handler a -> Handler a
requireLogin h = isLoggedIn >>= \r ->
                    case r of
                        True -> h
                        False -> redirect LoginR

-- | Is the current user logged in? This is a low level request - you're
--   probably looking for 'requireLogin'.
isLoggedIn :: Handler Bool
isLoggedIn = do lookupSession sessionKey >>= \key ->
                  case key of
                     Nothing -> return False
                     Just pw -> checkPassword pw

-- | Is the password correct?
checkPassword :: Text -> Handler Bool
checkPassword pw = (pw ==) <$> getGlobalPassword <$> getYesod

getGlobalPassword :: App -> Text
getGlobalPassword = extraPassword . appExtra . settings

-- | Adds any password (correct or not) to the user's session. This will
--   overwrite any previously-set value.
addPasswordToSession :: Text -> Handler ()
addPasswordToSession = setSession sessionKey

-- | Removes an entered password from the user's session.
removePasswordFromSession :: Handler ()
removePasswordFromSession = deleteSession sessionKey
