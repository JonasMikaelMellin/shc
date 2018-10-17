{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes                #-}
module Handler.Profile where 

import Import
import Data.CaseInsensitive (mk)
import Data.Aeson (Value)
import Data.Aeson.Types (typeMismatch)  
import Database.Persist.Sql (fromSqlKey,sqlQQ,executeQQ)
import Text.Julius (RawJS (..))
import Handler.Preferences (getAllPreferences)
import qualified Data.Tree.RBTree as RBT


data ProfileChangeLanguageRequest = ProfileChangeLanguageRequest {
  requestLanguageCode :: Text
  } deriving (Generic, Eq, Show)

instance FromJSON  ProfileChangeLanguageRequest
 
instance ToJSON ProfileChangeLanguageRequest
 
data ProfileChangeLanguageResponse = ProfileChangeLanguageResponse {
  reponseResult :: Bool
  } deriving (Generic, Eq, Show)

instance FromJSON  ProfileChangeLanguageResponse
 
instance ToJSON ProfileChangeLanguageResponse

data ProfileGetJavascriptI18NText = ProfileGetJavascriptI18NText {
  requestKey :: Text
  } deriving (Generic, Eq, Show)

instance FromJSON ProfileGetJavascriptI18NText

instance ToJSON ProfileGetJavascriptI18NText
 
getProfileR :: Handler Html   
getProfileR = do 
    (uid, user) <- requireAuthPair
    let uidSqlKey =  show $ fromSqlKey uid
    let profileLanguageChoice = "js-profileLanguageChoice" :: Text
    let profileRTitleId = "js-profileRTitle" :: Text
    allPreferences <- Handler.Preferences.getAllPreferences uid 
    _ <- case allPreferences of 
           [Entity _ preference] -> setLanguage $ preferencesLanguage preference
           _ -> setLanguage "sv"  
    master <- getYesod 
    defaultLayout $ do 
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "errorMessages")
        $(widgetFile "profile")
 
postProfileChangeLanguageR :: Handler Value
postProfileChangeLanguageR = do
  (uid, _) <- requireAuthPair
  pclr <- (requireJsonBody :: Handler ProfileChangeLanguageRequest) 
  let languageCode = requestLanguageCode pclr
  result <- if languageCode == (pack "sv") || languageCode == (pack "en") then 
              do
                setLanguage languageCode
                _ <- runDB $ [executeQQ|
                                   UPDATE ^{Preferences}  SET @{PreferencesLanguage} = #{languageCode} WHERE @{PreferencesUserId} = #{uid}
                                   |]
                return True
            else
              return False 
  case result of
    True -> returnJson $  ProfileChangeLanguageResponse result
    False -> sendResponseStatus status412 ("Invalid parameter"::Text)
    

-- initializeJavascriptMessages :: IORef (RBT.RBTree (Text, RBT.RBTree (Text,Text))) -> IO  (RBT.RBTree (Text, RBT.RBTree (Text,Text)))
-- initializeJavascriptMessages tree = do
--   master <- getYesod
--   --let concatenateElement etl = foldl append (pack "") $ map (\et -> let Element t = et in et) etl
--   result <- atomicModifyIORef tree (\c ->
--                                       let temp = RBT.insertOrdList c [
--                                             (pack "en",
--                                              RBT.insertOrdList RBT.emptyRB [
--                                                 (pack "ProfileRTitle"
--                                                 , pack $ "Access granted")
--                                                 ]
--                                             )
--                                             ,(pack "sv",
--                                               RBT.insertOrdList RBT.emptyRB [
--                                                  (pack "ProfileRTitle"
--                                                  , pack $ "Åtkomst medgiven")
--                                                  ]
--                                              )
--                                             ]
--                                       in
--                                           (temp
--                                             , temp
--                                             )
--                                    )
--   return result

     
postProfileGetJavascriptI18NText :: Handler Value
postProfileGetJavascriptI18NText = do
  (uid, _) <- requireAuthPair
  pgjit <- (requireJsonBody :: Handler ProfileGetJavascriptI18NText)
  master <- getYesod
  t <- liftIO $ atomicModifyIORef (appJavascriptMessages master) (\c -> (c,c))
  $(logDebug) $ pack $ show t
  returnJson pgjit
  
    
