{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Preferences where
 
import Import
import Data.Text (pack)

data PreferencesGetPreferencesRequest = PreferencesGetPreferencesRequest {
  preferencesGetPreferencesRequestUserId :: UserId
  } deriving (Generic, Eq, Show)
 
instance FromJSON PreferencesGetPreferencesRequest

instance ToJSON PreferencesGetPreferencesRequest

-- postPreferencesGetPreferencesR
-- Purpose: REST handle for getting preferences for a user.
-- Description: If no entry exists, then insert default record.
-- Input
--    Yesod default
-- Returns
--    Handler Value: HTTP code 200 if success
--    Handler Value: HTTP code 403 if CSRF failure
-- Exception
--    All Persist Exceptions

postPreferencesGetPreferencesR :: Handler Value
postPreferencesGetPreferencesR = do
  preferencesGetPreferencesRequest <- (requireJsonBody :: Handler PreferencesGetPreferencesRequest)
  --(uid, _) <- requireAuthPair
  let uid  = (preferencesGetPreferencesRequestUserId preferencesGetPreferencesRequest) 
    
  result <- getAllPreferences uid
  result' <- case result of
    [] -> do
      _ <- runDB $ insertEntity $ Preferences uid (Data.Text.pack "sv")
      getAllPreferences uid
    _ -> do return result
  returnJson result'


getAllPreferences :: UserId -> Handler [Entity Preferences]
getAllPreferences uid = do
  runDB $ selectList[PreferencesUserId ==. uid][]

