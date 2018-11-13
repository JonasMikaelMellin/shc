-- |
-- Module      : Handler.PrivacyPolicy
-- Copyright   : (c) 2018 Jonas Mellin
--
-- License     : GPLv3
--
-- Maintainer  : jonas.mellin@his.se
-- Stability   : experimental
-- Portability : Yesod
--
--
-- This handler module implements the back end functionality of team management in the
-- HITED software.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.PrivacyPolicy where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))


-- This is a handler function for the GET request method on the PrivacyPolicyR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getPrivacyPolicyR :: Handler Html
getPrivacyPolicyR = do
    setLanguage "sv"
    master <- getYesod
    defaultLayout $ do
        setTitle $ toHtml $ renderMessage master [] MsgPrivacyPolicyRTitle
        $(widgetFile "privacyPolicy")

 
