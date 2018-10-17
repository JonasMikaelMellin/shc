{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE QuasiQuotes                #-}
module Handler.User where
 
import Import  
import Text.Julius (RawJS (..))
import Data.CaseInsensitive (mk)
import Data.Aeson (Value)
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text,pack)
import Database.Persist.Sql (rawSql,sqlQQ,executeQQ)
import HITEDModule.Utilities (NullRequest,listOfStringsToSqlListString)
import Yesod.Static
 
   
data GetUserDataRequest = GetUserDataRequest { 
  userGetUserRequestUserId :: UserId
  , userGetUserRequestRowId :: Integer
  } deriving (Generic, Eq, Show)

instance FromJSON GetUserDataRequest

instance ToJSON GetUserDataRequest
 
 
data GetUserDataResponse = GetUserDataResponse {
  userGetUserResponseEntityUserData :: [Entity User]
  , userGetUserResponseRowId :: Integer
  } deriving (Generic)

instance FromJSON GetUserDataResponse

instance ToJSON GetUserDataResponse

data GetNotInTeamUsersRequest = GetNotInTeamUsersRequest {
  getNotInTeamUsersRequestTeamName :: Text
  , getNotInTeamUsersRequestRowId :: Integer
  } deriving (Generic, Eq, Show)

instance FromJSON GetNotInTeamUsersRequest

instance ToJSON GetNotInTeamUsersRequest

data GetNotInTeamUsersResponse = GetNotInTeamUsersResponse {
  getNotInTeamUsersResponseUserData :: [Entity User]
  , getNotInTeamUsersResponseRowId :: Integer
  } deriving (Generic)

instance FromJSON GetNotInTeamUsersResponse

instance ToJSON GetNotInTeamUsersResponse


data GetInTeamUsersRequest = GetInTeamUsersRequest {
  getInTeamUsersRequestTeamName :: Text
  , getInTeamUsersRequestRowId :: Integer
  } deriving (Generic, Eq, Show)

instance FromJSON GetInTeamUsersRequest

instance ToJSON GetInTeamUsersRequest

data GetInTeamUsersResponse = GetInTeamUsersResponse {
  getInTeamUsersResponseUserData :: [Entity User]
  , getInTeamUsersResponseRowId :: Integer
  } deriving (Generic)

instance FromJSON GetInTeamUsersResponse

instance ToJSON GetInTeamUsersResponse

    
-- getTeamR :: Handler Html
-- getTeamR = do
--     let (teamFormId, teamCreateEntryId, teamListId, teamCreateEntryDescriptionId, teamFormDeleteId, teamRegisterTeamButtonId,teamDeleteTeamButtonId) = teamHtmlIds 
--     maybeCurrentUserId <- maybeAuthId      
--     allTeams <- getAllTeamsForUser maybeCurrentUserId
--     defaultLayout $ do 
--         aDomId <- newIdent     
--         master <- getYesod       
--         setTitle $ toHtml $ renderMessage master [] MsgTeamRTitle
        
--         $(widgetFile "team")    
              
-- postUserR :: Handler Value  
-- postUserR = do 
--   proceed <- hasValidCsrfHeaderNamed $ mk $ defaultCsrfCookieName  
--   case proceed of  
--     True -> do   
--       maybeCurrentUserId <- maybeAuthId
--       userId <- (requireJsonBody :: Handler Team)
--       case maybeCurrentUserId of 
--         Just userId -> do  
--           $(logDebug) "Found user id"  
--           let team' = team { teamOwner = userId }
--           insertedTeam <- runDB $ insertEntity team' 
--           returnJson insertedTeam
--         Nothing -> sendResponseStatus status403 ("Not logged in"::Text)
        
--     False -> do
--       $(logDebug) "No user id"
--       sendResponseStatus status403 ("CSRF Failure"::Text)

-- postUserGetUserR
-- Purpose: REST handle for retrieving user data
-- Input
--   Yesod default
-- Returns
--   Handler Value: GetUserDataResponse, if success
--   Handler Value: HTTP code 403 if CSRF failure
-- Exceptions
--   All Persist exceptions

postUserGetUserR :: Handler Value    
postUserGetUserR = do     
  proceed <- hasValidCsrfHeaderNamed $ mk $ defaultCsrfCookieName
  case proceed of
    True -> do
      getUserDataRequest <- (requireJsonBody :: Handler GetUserDataRequest)
      userData <-  getUserData $ userGetUserRequestUserId getUserDataRequest 
      let result = GetUserDataResponse {
            userGetUserResponseEntityUserData = userData,
            userGetUserResponseRowId = userGetUserRequestRowId getUserDataRequest }
      returnJson result
    False -> do   
      sendResponseStatus status403 ("CSRF Failure"::Text)

postUserGetNotInTeamUsersR :: Handler Value
postUserGetNotInTeamUsersR = do
  (uid,_) <- requireAuthPair
  getNotInTeamUsersRequest <- (requireJsonBody :: Handler GetNotInTeamUsersRequest)
  userData <-  getNotInTeamUsers (getNotInTeamUsersRequestTeamName getNotInTeamUsersRequest) uid
  let result = GetNotInTeamUsersResponse {
        getNotInTeamUsersResponseUserData = userData,
        getNotInTeamUsersResponseRowId = getNotInTeamUsersRequestRowId  getNotInTeamUsersRequest}
  returnJson result


postUserGetInTeamUsersR :: Handler Value
postUserGetInTeamUsersR = do
  (uid,_) <- requireAuthPair
  getInTeamUsersRequest <- (requireJsonBody :: Handler GetInTeamUsersRequest)
  userData <-  getInTeamUsers (getInTeamUsersRequestTeamName getInTeamUsersRequest) uid
  let result = GetInTeamUsersResponse {
        getInTeamUsersResponseUserData = userData,
        getInTeamUsersResponseRowId = getInTeamUsersRequestRowId  getInTeamUsersRequest}
  returnJson result

-- postTeamGetTeamMembersR :: Handler Value 
-- postTeamGetTeamMembersR = do
--   proceed <- hasValidCsrfHeaderNamed $ mk $ defaultCsrfCookieName
--   case proceed of
--     True -> do 
--       getTeamMember <- (requireJsonBody :: Handler GetTeamMember)
--       allTeamMembers <-   getAllTeamMembersForTeam $ tid getTeamMember
--       returnJson allTeamMembers
--     False -> do
--       sendResponseStatus status403 ("CSRF Failure"::Text)

-- postTeamRemoveTeamR
-- Purpose: REST handle for removing projects
-- Input
--   Yesod default
-- Returns
--   Handler Value: HTTP code 200 if success
--   Handler Value: HTTP code 403 if CSRF failure
-- Exceptions     
--   All Persist exceptions    

-- postTeamRemoveTeamsR :: Handler Value 
-- postTeamRemoveTeamsR = do
--   proceed <- hasValidCsrfHeaderNamed $ mk $ defaultCsrfCookieName
--   case proceed of
--     True -> do
--       removeTeamList <- (requireJsonBody :: Handler RemoveTeamList)
--       maybeCurrentUserId <- maybeAuthId
--       _ <- removeTeams (listOfTeams removeTeamList) maybeCurrentUserId
--       sendResponseStatus status200 ("Success"::Text)
--     False -> do
--       sendResponseStatus status403 ("CSRF Failure"::Text)
  
 
-- Support functions in project page 
      
getUserData :: UserId -> Handler [Entity User]
getUserData u = do
  runDB $ [sqlQQ|
                SELECT ?? FROM ^{User} WHERE @{UserId} = #{u}
                |]

getNotInTeamUsers :: Text -> UserId -> Handler [Entity User]
getNotInTeamUsers t owner = do
  runDB $ rawSql q [toPersistValue t,toPersistValue owner] where
    q = "SELECT ?? FROM user WHERE user.id NOT IN (SELECT user_id FROM team_member WHERE team_member.team_id IN (SELECT team.id FROM team WHERE team.name = ?)) AND user.id <> ? ORDER BY user.ident";


getInTeamUsers :: Text -> UserId -> Handler [Entity User]
getInTeamUsers t owner = do
  runDB $ rawSql q [toPersistValue t,toPersistValue owner] where
    q = "SELECT ?? FROM user WHERE user.id IN (SELECT user_id FROM team_member WHERE team_member.team_id IN (SELECT team.id FROM team WHERE team.name = ?)) OR user.id = ? ORDER BY user.ident";




 




  

