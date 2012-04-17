{-# LANGUAGE OverloadedStrings #-}
module Handler.Logout ( getLogoutR
                      ) where

import Import
import Auth

getLogoutR :: Handler ()
getLogoutR = do removePasswordFromSession
                redirect LoginR
