-- |
-- Module      : Handler.Team
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


{-# LANGUAGE DeriveGeneric #-}     
{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}  
{-# LANGUAGE RecordWildCards #-}    
{-# LANGUAGE QuasiQuotes                #-}    
module Handler.Team where   
               
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

data AugmentedTeamData = AugmentedTeamData {
  augmentedTeamDataEntityTeam :: Entity Team
  , augmentedTeamDataIsOwner :: Bool
  } deriving (Generic)

instance FromJSON AugmentedTeamData

instance ToJSON AugmentedTeamData

data TeamGetTeamsResponse = TeamGetTeamsResponse {
  teamGetTeamsResponseAugmentedTeamData :: [AugmentedTeamData]
  } deriving (Generic)

instance FromJSON TeamGetTeamsResponse

instance ToJSON TeamGetTeamsResponse

data TeamAddTeamRequest = TeamAddTeamRequest {
  teamAddTeamRequestName :: Text
  , teamAddTeamRequestDescription :: Text
  } deriving (Generic, Eq, Show)

instance FromJSON TeamAddTeamRequest

instance ToJSON TeamAddTeamRequest
     
data TeamRemoveTeamListRequest = TeamRemoveTeamListRequest {  
  teamRemoveTeamListRequestUuidList :: [Data.UUID.UUID]     
  } deriving (Generic, Eq, Show)      
    
instance FromJSON TeamRemoveTeamListRequest 
   
instance ToJSON TeamRemoveTeamListRequest 

data TeamGetTeamMembersRequest = TeamGetTeamMembersRequest {
  teamGetTeamMembersRequestTeamId :: TeamId
  , teamGetTeamMembersRequestRowId :: Integer
  } deriving (Generic, Eq, Show)

instance FromJSON TeamGetTeamMembersRequest 
 
instance ToJSON TeamGetTeamMembersRequest

data GetTeamMembersResponse = GetTeamMembersResponse {
  responseEntityTeamData :: [Entity User] 
  , responseRowId :: Integer
  } deriving (Generic)

instance FromJSON GetTeamMembersResponse 

instance ToJSON GetTeamMembersResponse

data TeamAddUserToTeamRequest = TeamAddUserToTeamRequest {
  teamAddUserToTeamRequestUserIds :: [UserId]
  , teamAddUserToTeamRequestTeamUuid :: Data.UUID.UUID
  } deriving (Generic)

instance FromJSON TeamAddUserToTeamRequest

instance ToJSON TeamAddUserToTeamRequest

data TeamAddUserToTeamResponse = TeamAddUserToTeamResponse {
  teamAddUserToTeamResponseResult :: Bool
  } deriving (Generic)

instance FromJSON TeamAddUserToTeamResponse

instance ToJSON TeamAddUserToTeamResponse

data TeamRemoveUserFromTeamRequest = TeamRemoveUserFromTeamRequest {
  teamRemoveUserFromTeamRequestUserIds :: [UserId]
  , teamRemoveUserFromTeamRequestTeamUuid :: Data.UUID.UUID
  } deriving (Generic)

instance FromJSON TeamRemoveUserFromTeamRequest

instance ToJSON TeamRemoveUserFromTeamRequest 

data TeamRemoveUserFromTeamResponse = TeamRemoveUserFromTeamResponse {
  teamRemoveUserFromTeamResponseResult :: Bool
  } deriving (Generic)

instance FromJSON TeamRemoveUserFromTeamResponse

instance ToJSON TeamRemoveUserFromTeamResponse

data TeamUpdateTeamRequest = TeamUpdateTeamRequest {
  teamUpdateTeamRequestTeamUuid :: Data.UUID.UUID
  , teamUpdateTeamRequestTeamName :: Text
  , teamUpdateTeamRequestTeamDescription :: Text
  } deriving (Generic)

instance FromJSON TeamUpdateTeamRequest

instance ToJSON TeamUpdateTeamRequest 

      
teamHtmlIds :: (Text, Text, Text, Text, Text, Text, Text)   
teamHtmlIds = ("js-teamForm"
              , "js-createTeamEntry"
              , "js-teamList"
              , "js-createTeamEntryDescription"
              , "js-registerTeamButton"
              , "js-deleteTeamButton"
              , "js-teamModslAddPoint")   
    
getTeamR :: Handler Html  
getTeamR = do
  let (teamFormId, teamCreateEntryId, teamListId, teamCreateEntryDescriptionId, teamRegisterTeamButtonId,teamDeleteTeamButtonId,teamModalAddPointId) = teamHtmlIds 
  (uid, _) <- requireAuthPair
  allTeams <- getAllTeamsForUser uid
  hitedSetLanguage
  let uidSqlKey =  show $ fromSqlKey uid
  defaultLayout $ do    
      aDomId <- newIdent     
      master <- getYesod       
      setTitle $ toHtml $ renderMessage master [] MsgTeamRTitle
      $(widgetFile "errorMessages")
      $(widgetFile "team")

 
              
postTeamR :: Handler Value  
postTeamR = do
  teamAddTeamRequest <- (requireJsonBody :: Handler TeamAddTeamRequest)    
  (uid,_) <- requireAuthPair
  let uuid = Data.UUID.V4.nextRandom
  let uuidText = unsafePerformIO $ liftM Data.UUID.toText uuid
  let team' = Team (teamAddTeamRequestName teamAddTeamRequest) uid (teamAddTeamRequestDescription teamAddTeamRequest) uuidText
  insertedTeam <- runDB $ insertEntity team'      
  returnJson insertedTeam 
        
         
postTeamGetTeamsR :: Handler Value     
postTeamGetTeamsR = do     
  _ <- (requireJsonBody :: Handler HITEDModule.Utilities.NullRequest)
  (uid,_) <- requireAuthPair
  allTeams <-   getAllTeamsForUser uid
  allTeamsInWhichUserOnlyParticipates <- getAllProjectsInWhichUserOnlyParticipates uid
  -- define the local function transform that construct AugmentedProjectData based on
  -- the Entity Project and sets the augmentedProjectDataIsOwner flag to True if
  -- the owner is the same as the user  
  let transform = \et -> AugmentedTeamData {
        augmentedTeamDataEntityTeam = et
        , augmentedTeamDataIsOwner = case et of
            Entity _ teamRecord -> uid == (teamOwner teamRecord)
        }
  -- apply transform to the merge of both results
  let result = fmap transform (allTeams ++ allTeamsInWhichUserOnlyParticipates)

  returnJson result
 
postTeamGetTeamMembersR :: Handler Value 
postTeamGetTeamMembersR = do
  proceed <- hasValidCsrfHeaderNamed $ mk $ defaultCsrfCookieName
  case proceed of
    True -> do 
      getTeamMembersRequest <- (requireJsonBody :: Handler TeamGetTeamMembersRequest)
      $(logDebug) $ Data.Text.pack $ show $ teamGetTeamMembersRequestTeamId getTeamMembersRequest
      allTeamMembers <-   getAllTeamMembersForTeam $ teamGetTeamMembersRequestTeamId getTeamMembersRequest
      $(logDebug) $ Data.Text.pack $ show $ length allTeamMembers
      $(logDebug) $ Data.Text.pack $ foldr (++) "" $ map (\t -> (show t)++", ") allTeamMembers
      let result = GetTeamMembersResponse {
            responseEntityTeamData = allTeamMembers,
            responseRowId = teamGetTeamMembersRequestRowId getTeamMembersRequest
            }
      returnJson result
    False -> do
      sendResponseStatus status403 ("CSRF Failure"::Text) 
   
-- | postTeamRemoveTeamR
-- Purpose: REST handle for removing projects
-- Input
--   Yesod default
-- Returns
--   Handler Value: HTTP code 200 if success
--   Handler Value: HTTP code 4XX if failure
-- Exceptions     
--   All Persist exceptions    

postTeamRemoveTeamsR :: Handler Value 
postTeamRemoveTeamsR = do
  removeTeamList <- (requireJsonBody :: Handler TeamRemoveTeamListRequest)
  maybeCurrentUserId <- maybeAuthId
  _ <- removeTeams (teamRemoveTeamListRequestUuidList removeTeamList) maybeCurrentUserId
  sendResponseStatus status200 ("Success"::Text)



-- | postTeamAddUserToTeamR
-- Purpose: REST handle for adding users to team
-- Input
--   Yesod default - Handler TeamAddUserToTeamRequest
-- Returns
--   Handler Value: HTTP code 200 if success -  TeamAddUserToTeamResponse
--   Handler Value: HTTP code 4XX if failure
-- Exceptions     
--   All Persist exceptions    

postTeamAddUserToTeamR :: Handler Value
postTeamAddUserToTeamR = do
  teamAddUserToTeamRequest <- (requireJsonBody :: Handler TeamAddUserToTeamRequest)
  let userIds = teamAddUserToTeamRequestUserIds teamAddUserToTeamRequest
  let teamUuid = teamAddUserToTeamRequestTeamUuid teamAddUserToTeamRequest
  _ <- addUserToTeam userIds teamUuid
  let result = TeamAddUserToTeamResponse True
  returnJson result

-- | postTeamRemoveUserFromTeamR
-- Purpose: REST handle for removing users from team
-- Input
--   Yesod default - HandlerTeamRemoveUserFromTeamRequest
-- Returns
--   Handler Value: HTTP code 200 if success - TeamRemoveUserFromTeamResponse
--   Handler Value: HTTP code 4XX if failure
-- Exceptions     
--   All Persist exceptions    

postTeamRemoveUserFromTeamR :: Handler Value
postTeamRemoveUserFromTeamR = do
  teamRemoveUserFromTeamRequest <- (requireJsonBody :: Handler TeamRemoveUserFromTeamRequest)
  let userIds = teamRemoveUserFromTeamRequestUserIds teamRemoveUserFromTeamRequest
  let teamUuid = teamRemoveUserFromTeamRequestTeamUuid teamRemoveUserFromTeamRequest
  _ <- removeUserFromTeam userIds teamUuid
  let result = TeamRemoveUserFromTeamResponse True
  returnJson result


-- | postTeamUpdateTeamR
-- Purpose: REST handle for updating team data
-- Input
--   Yesod default - Handler TeamUpdateTeamRequest
-- Returns
--   Handler Value: HTTP code 200 if success - [Entity Project]
--   Handler Value: HTTP code 4XX if failure
-- Exceptions     
--   All Persist exceptions    

postTeamUpdateTeamR :: Handler Value
postTeamUpdateTeamR = do
  teamUpdateTeamRequest <- (requireJsonBody :: Handler TeamUpdateTeamRequest)
  let teamUuid = teamUpdateTeamRequestTeamUuid teamUpdateTeamRequest
  let teamName = teamUpdateTeamRequestTeamName teamUpdateTeamRequest
  let teamDescription = teamUpdateTeamRequestTeamDescription teamUpdateTeamRequest
  _ <- updateTeam teamUuid teamName teamDescription
  (uid,_) <- requireAuthPair
  allTeams <-   getAllTeamsForUser uid
  returnJson allTeams
  
  
-- Support functions in project page 
      
 
-- | getAllTeams
-- Purpose: obtain all teams available in the database and return a list
-- Input
--  N/A
-- Returns
--  [Entity Project]
-- Exceptions
--  All Persist exceptions
 
getAllTeams :: Handler [Entity Team]
getAllTeams = do
  runDB $ selectList [] [Asc TeamName] 

-- | getAllTeamsForUser
-- Purpose: obtain all teams available to the logged in user
-- Input
--  maybeCurrentUserId: Maybe UserId: If <> Nothing, then the user identity
-- Returns
--  [Entity Team]: A list of projects
-- Exceptions
--  All Persist exceptions


getAllTeamsForUser :: UserId -> Handler [Entity Team]  
getAllTeamsForUser userId = do 
  runDB $ [sqlQQ|
                SELECT ?? FROM ^{Team} WHERE @{TeamOwner} = #{userId} ORDER BY @{TeamName}
                |]

getAllProjectsInWhichUserOnlyParticipates :: UserId -> Handler [Entity Team]
getAllProjectsInWhichUserOnlyParticipates userId = do
  runDB $ rawSql q [toPersistValue userId] where
    q="SELECT ?? FROM team WHERE team.id IN (SELECT team_member.team_id FROM team_member WHERE user_id = ?) ORDER BY team.name"
 

-- | getAllTeamsMembersForTeam
-- Purpose: obtain all team members for the team
-- Input
--  teamId: TeamId: If <> Nothing, then the user identity
-- Returns
--  [Entity User]: A list of projects
-- Exceptions
--  All Persist exceptions

getAllTeamMembersForTeam :: TeamId -> Handler [Entity User]
getAllTeamMembersForTeam  teamId = do
  runDB $ rawSql q [toPersistValue teamId] where
    q = "SELECT ?? FROM user WHERE user.id IN (SELECT user_id FROM team_member WHERE team_member.team_id = ?) ORDER BY user.ident";

     
 
   
-- removeTeams
-- Purpose: remove teams owned by the user
-- Input
--   listOfTeam: [Data.UUID.UUID]: List of UUIDs of projects
--   maybeCurrentUserId: Maybe UserId: If <> Nothing, then the user identity
-- Returns
--   () 
-- Exceptions
--   All Persist exceptions


removeTeams :: [Data.UUID.UUID] -> Maybe UserId -> Handler ()
removeTeams listOfTeams maybeCurrentUserId = do
 
  case maybeCurrentUserId of
    Just userId -> do
      let removeTeams' = \lop ->
            case lop of
              x:xs -> do
                runDB $ [executeQQ|
                                  DELETE FROM ^{Team} WHERE @{TeamOwner} = #{userId} AND @{TeamUuid} =  #{Data.UUID.toText x}
                                  |]
                removeTeams' xs
              [] -> do
                return ()
        
      removeTeams' listOfTeams
    Nothing -> do
      return ()

-- | addUserToTeam
-- Purpose: add users to team
-- Input
--   userIds: [UserId]: List of user identities
--   teamUuid: Data.UUID.UUID: UUID of the team
-- Returns
--   () 
-- Exceptions
--   All Persist exceptions

addUserToTeam :: [UserId] -> Data.UUID.UUID -> Handler ()
addUserToTeam userIds teamUuid = do
  let teamUuidText = Data.UUID.toText teamUuid
  maybeEntityTeam <- runDB $ selectFirst[TeamUuid ==. teamUuidText][]
  case maybeEntityTeam of
    Just (Entity teamId _) -> do
      let addUserToTeam' = \userId ->
            case userId of
              u:us -> do
                _ <- runDB $ insert (TeamMember u teamId)
                addUserToTeam' us
              [] -> do
                return ()
      addUserToTeam' userIds
    Nothing -> do
      return ()

-- | removeUserFromTeam
-- Purpose: remove users from team
-- Input
--   userIds: [UserId]: List of user identities
--   teamUuid: Data.UUID.UUID: UUID of the team
-- Returns
--   () 
-- Exceptions
--   All Persist exceptions

removeUserFromTeam :: [UserId] -> Data.UUID.UUID -> Handler ()
removeUserFromTeam userIds teamUuid = do
  let teamUuidText = Data.UUID.toText teamUuid
  maybeEntityTeam <- runDB $ selectFirst[TeamUuid ==. teamUuidText][]
  case maybeEntityTeam of
    Just (Entity teamId _) -> do
      let removeUserFromTeam' = \userId ->
            case userId of
              u:us -> do
                _ <- runDB $ deleteWhere[TeamMemberUserId ==. u, TeamMemberTeamId ==. teamId]
                removeUserFromTeam' us
              [] -> do
                return ()
      removeUserFromTeam' userIds
    Nothing -> do
      return ()

updateTeam :: Data.UUID.UUID -> Text -> Text -> Handler ()
updateTeam teamUuid teamName teamDescription = do
  _ <- runDB $ [executeQQ|
                         UPDATE ^{Team}
                         SET @{TeamName} = #{teamName},
                         @{TeamDescription} = #{teamDescription}
                         WHERE @{TeamUuid} = #{Data.UUID.toText teamUuid}
                         |]
  return ()
  




  

