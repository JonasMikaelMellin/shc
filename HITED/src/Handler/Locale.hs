{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes                #-}
module Handler.Locale where
 
import Import
import Data.CaseInsensitive (mk)
import Data.Aeson (Value)
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text,pack)


data LocaleParameter = LocaleParameter {
  localeParameter :: [Either String Int]
  } deriving (Eq, Read, Show);

instance FromJSON LocaleParameter where
  parseJSON (Object v) = LocaleParameter
    <$> v .: "localeParameter"
  parseJSON invalid = typeMismatch "LocaleParameter" invalid

instance ToJSON LocaleParameter where
  toJSON LocaleParameter {..} = object
    [ "localeParameter" .= localeParameter ]

postLocaleTransformMessageR :: Handler Value
postLocaleTransformMessageR = do 
  proceed <- hasValidCsrfHeaderNamed $ mk $ defaultCsrfCookieName
  case proceed of 
    True -> do  
      localeParameter <- (requireJsonBody :: Handler LocaleParameter)
      sendResponseStatus status403 ("Not logged in"::Text)
        
    False -> do
      $(logDebug) "No user id"
      sendResponseStatus status403 ("CSRF Failure"::Text)
