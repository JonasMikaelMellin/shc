{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies #-}
module HITEDModule.Utilities
  ( NullRequest
  , getUUIDV4AsText
  , hitedSetLanguage
  , listOfStringsToSqlListString
  ) where

import Import
import Data.Aeson.Types (typeMismatch)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as UV4
import System.IO.Unsafe (unsafePerformIO) 


data NullRequest = NullRequest {
  requestId :: Int
  } deriving (Eq, Show)

instance ToJSON NullRequest where
  toJSON NullRequest {..} = object
    [ "requestId" .= requestId ]

instance FromJSON NullRequest where
  parseJSON (Object v) = NullRequest
    <$> v .: "requestId"

  parseJSON invalid = typeMismatch  "NullRequest" invalid


getUUIDV4AsText :: Text
getUUIDV4AsText = ""

commatizeAndMerge :: [Text] ->    Text
commatizeAndMerge lop  =
  case lop of
    [x] -> x
    x:xs ->
      let
        part1 = unpack $ x
        part2 = unpack $ commatizeAndMerge $ xs
        result = part1 ++ "," ++ part2
      in
        pack $ result
    _ -> ""

listOfStringsToSqlListString ::  [Text] ->   Text
listOfStringsToSqlListString listOfProjects = 
  let
    result = unpack $ commatizeAndMerge listOfProjects
    presult = "(" ++ result ++ ")"
  in
    pack $ presult

hitedSetLanguage :: Handler ()
hitedSetLanguage = do
  (uid, _) <- requireAuthPair
  maybePreferences <- runDB $ selectFirst[PreferencesUserId ==. uid][]
  let languagePreferences = case maybePreferences of
        Just (Entity _ preferences) -> preferences
        Nothing -> Preferences uid "sv"
  setLanguage $ preferencesLanguage $ languagePreferences
  return ()
