{-# LANGUAGE OverloadedStrings #-}
module Handler.Login ( getLoginR
                     , postCheckPassR
                     ) where

import Auth ( isLoggedIn
            , checkPassword
            , addPasswordToSession
            )
import Import

title :: Html
title = "Login"

getLoginR :: Handler RepHtml
getLoginR = do loggedIn <- isLoggedIn
               if loggedIn
                 then redirect HomeR
                 else defaultLayout $ setTitle title
                                    >> $(widgetFile "login")

-- | POST to this URL with pw=your_escaped_password to get back a JSON of
--   whether or not that password is correct.
--
--   TODO: Exponential time delays to prevent bruteforcers.
postCheckPassR :: Handler RepJson
postCheckPassR = do pw <- runInputPost $ ireq textField "pw"
                    rightPw <- checkPassword pw
                    case rightPw of
                        False -> permissionDenied "Bad Password."
                        True  -> do addPasswordToSession pw
                                    sendResponse $ RepJson emptyContent
