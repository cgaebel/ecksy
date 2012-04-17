{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Config ( getConfigR
                      , postConfigR
                      ) where

import Import

{-title :: Html
title = "Config"-}

getConfigR :: Handler RepHtml
getConfigR = redirect HomeR

postConfigR :: Handler RepHtml
postConfigR = redirect HomeR
