-- |
-- Module      : Handler.Editor
-- Copyright   : (c) 2018 Jonas Mellin
--
-- License     : GPLv3
--
-- Maintainer  : jonas.mellin@his.se
-- Stability   : experimental
-- Portability : Yesod
--
--
-- This module is the main editor of models and experiments
-- HITED software.


{-# LANGUAGE DeriveGeneric #-}     
{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}  
{-# LANGUAGE RecordWildCards #-}    
{-# LANGUAGE QuasiQuotes                #-}    
module Handler.Editor where   
               
import Import        
import Text.Julius (RawJS (..))      
import Data.CaseInsensitive (mk)      
import Data.Aeson (Value)
import Data.Text (Text,pack)   
import qualified Data.UUID
import qualified Data.UUID.V4
import Database.Persist.Sql (sqlQQ,executeQQ,fromSqlKey,rawSql) 
import HITEDModule.Utilities (NullRequest,hitedSetLanguage,getUUIDV4AsText)
import System.IO.Unsafe (unsafePerformIO) 


      
editorHtmlIds :: (Text, Text, Text, Text, Text, Text, Text)   
editorHtmlIds = ("js-editorTabId"
                , "js-editorNavigationBarId"
                , "js-editorModelPaletteId"
                , "js-editorExperimentPaletteId"
                , "js-editorModelId"
                , "js-editorExperimentId"
                , "js-editorModalAddPoint")   
    
getEditorR :: Handler Html  
getEditorR = do
  let (editTabId
        , editorNavigationBarId
        , editorModelPaletteId
        , editorExperimentPaletteId
        , editorModelId
        , editorExperimentId,
          editorModalAddPointId) = editorHtmlIds 
  (uid, _) <- requireAuthPair
  hitedSetLanguage
  master <- getYesod
  let as = appSettings master
  let editorTabHeight = appEditorTabHeight as
  let editorNavigationHeight = appEditorNavigationHeight as
  let editorPaletteHeight = appEditorPaletteHeight as
  let editorModelHeight = appEditorModelHeight as 
  defaultLayout $ do    
      aDomId <- newIdent     
      setTitle $ toHtml $ renderMessage master [] MsgTeamRTitle
      $(widgetFile "errorMessages")
      $(widgetFile "editor") 

 
