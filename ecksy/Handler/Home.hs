{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home ( getHomeR ) where

import Auth
import Import

title :: Html
title = "Home"

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = requireLogin . defaultLayout $ do
    setTitle title
    $(widgetFile "homepage")
