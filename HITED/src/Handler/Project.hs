{-# LANGUAGE DeriveGeneric #-}     
{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes                #-}
module Handler.Project where
    
import Import  
import Text.Julius (RawJS (..))
import Data.CaseInsensitive (mk)
import Data.Aeson (Value)
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text,pack)
import qualified Data.UUID
import qualified Data.UUID.V4
import Database.Persist.Sql (rawSql,sqlQQ,executeQQ,fromSqlKey)
import HITEDModule.Utilities (NullRequest,listOfStringsToSqlListString,hitedSetLanguage,getUUIDV4AsText)
import System.IO.Unsafe (unsafePerformIO)

data AugmentedProjectData = AugmentedProjectData {
  augmentedProjectDataEntityProject :: Entity Project
  , augmentedProjectDataIsOwner :: Bool
  } deriving (Generic)

instance FromJSON AugmentedProjectData

instance ToJSON AugmentedProjectData

data ProjectGetProjectsResponse = ProjectGetProjectsResponse {
  projectGetProjectResponseAugmentedProjectData :: [AugmentedProjectData]
  } deriving (Generic)

instance FromJSON ProjectGetProjectsResponse

instance ToJSON ProjectGetProjectsResponse

data ProjectAddProjectRequest = ProjectAddProjectRequest {
  projectAddProjectRequestName :: Text
  , projectAddProjectRequestDescription :: Text
  } deriving (Generic, Eq, Show)

instance FromJSON ProjectAddProjectRequest

instance ToJSON ProjectAddProjectRequest  
 
   
data ProjectRemoveProjectsRequest = ProjectRemoveProjectsRequest { 
  projectRemoveProjectsRequestListOfProjects :: [Data.UUID.UUID]  
  } deriving (Generic, Eq, Show)
 
instance FromJSON ProjectRemoveProjectsRequest

instance ToJSON ProjectRemoveProjectsRequest

data ProjectGetAssociatedTeamsRequest = ProjectGetAssociatedTeamsRequest {
  projectGetAssociatedTeamsRequestProjectId :: ProjectId
  , projectGetAssociatedTeamsRequestRowId :: Integer
  } deriving (Generic, Eq, Show)

instance FromJSON ProjectGetAssociatedTeamsRequest
    
instance ToJSON ProjectGetAssociatedTeamsRequest

data ProjectGetAssociatedTeamsResponse = ProjectGetAssociatedTeamsResponse {
  projectGetAssociatedTeamsResponseEntityTeamData :: [Entity Team] 
  , projectGetAssociatedTeamsResponseRowId :: Integer
  } deriving (Generic)

instance FromJSON  ProjectGetAssociatedTeamsResponse

instance ToJSON ProjectGetAssociatedTeamsResponse

data ProjectUpdateProjectRequest = ProjectUpdateProjectRequest {
  projectUpdateProjectRequestProjectUuid :: Data.UUID.UUID
  , projectUpdateProjectRequestProjectName :: Text
  , projectUpdateProjectRequestProjectDescription :: Text
  } deriving (Generic, Eq, Show)
  
instance FromJSON ProjectUpdateProjectRequest

instance ToJSON ProjectUpdateProjectRequest

data ProjectAddTeamToProjectRequest = ProjectAddTeamToProjectRequest {
  projectAddTeamToProjectRequestTeamIds :: [TeamId]
  , projectAddTeamToProjectRequestProjectUuid :: Data.UUID.UUID
  } deriving (Generic)

instance FromJSON ProjectAddTeamToProjectRequest

instance ToJSON ProjectAddTeamToProjectRequest

data ProjectAddTeamToProjectResponse = ProjectAddTeamToProjectResponse {
  projectAddTeamToProjectResponseResult :: Bool
  } deriving (Generic)

instance FromJSON ProjectAddTeamToProjectResponse

instance ToJSON ProjectAddTeamToProjectResponse

data ProjectRemoveTeamFromProjectRequest =  ProjectRemoveTeamFromProjectRequest{
  projectRemoveTeamFromProjectRequestTeamIds :: [TeamId]
  , projectRemoveTeamFromProjectRequestProjectUuid :: Data.UUID.UUID
  } deriving (Generic)

instance FromJSON ProjectRemoveTeamFromProjectRequest

instance ToJSON  ProjectRemoveTeamFromProjectRequest

data ProjectRemoveTeamFromProjectResponse = ProjectRemoveTeamFromProjectResponse {
  projectRemoveTeamFromProjectResponseResult :: Bool
  } deriving (Generic)

instance FromJSON ProjectRemoveTeamFromProjectResponse
  
instance ToJSON ProjectRemoveTeamFromProjectResponse

data ProjectGetNotInProjectTeamsRequest = ProjectGetNotInProjectTeamsRequest {
  projectGetNotInProjectTeamsRequestProjectUuid :: Data.UUID.UUID
  , projectGetNotInProjectTeamsRequestRowIndex :: Integer
  } deriving (Generic, Eq, Show)

instance FromJSON ProjectGetNotInProjectTeamsRequest
     
instance ToJSON ProjectGetNotInProjectTeamsRequest

data ProjectGetNotInProjectTeamsResponse = ProjectGetNotInProjectTeamsResponse {
  projectGetNotInProjectTeamsResponseTeamData :: [Entity Team]
  ,  projectGetNotInProjectTeamsResponseRowIndex :: Integer
  } deriving (Generic)

instance FromJSON ProjectGetNotInProjectTeamsResponse

instance ToJSON ProjectGetNotInProjectTeamsResponse


data ProjectGetInProjectTeamsRequest = ProjectGetInProjectTeamsRequest {
  projectGetInProjectTeamsRequestProjectUuid :: Data.UUID.UUID
  , projectGetInProjectTeamsRequestRowIndex :: Integer
  } deriving (Generic, Eq, Show)

instance FromJSON ProjectGetInProjectTeamsRequest

instance ToJSON ProjectGetInProjectTeamsRequest

data ProjectGetInProjectTeamsResponse =  ProjectGetInProjectTeamsResponse {
  projectGetInProjectTeamsResponseTeamData :: [Entity Team]
  , projectGetInProjectTeamsResponseRowIndex :: Integer
  } deriving (Generic)

instance FromJSON ProjectGetInProjectTeamsResponse

instance ToJSON ProjectGetInProjectTeamsResponse


projectHtmlIds :: (Text, Text, Text, Text, Text, Text, Text, Text)   
projectHtmlIds = ("js-projectForm",
                  "js-createProjectEntry",
                  "js-projectList",
                  "js-createProjectEntryDescription",
                  "js-projectFormDelete",
                  "js-registerProjectButton",
                  "js-deleteProjectButton",
                  "js-projectModalAddPoint")
 
 
getProjectListR :: Handler Html
getProjectListR = do
    let (projectFormId, projectCreateEntryId, projectListId, projectCreateEntryDescriptionId, projectFormDeleteId, projectRegisterProjectButtonId,projectDeleteProjectButtonId, projectModalAddPointId) = projectHtmlIds 
    (uid,_) <- requireAuthPair
    hitedSetLanguage
    let uidSqlKey = show $ fromSqlKey uid
    allProjects <- getAllProjectsForUser uid
    master <- getYesod
    defaultLayout $ do
      aDomId <- newIdent  
      setTitle "HITED:  Projektlista"
      
      $(widgetFile "errorMessages")

      $(widgetFile "projectList")   
  
postProjectListR :: Handler Value
postProjectListR = do
  (uid,_) <- requireAuthPair
  projectAddProjectRequest <- (requireJsonBody :: Handler ProjectAddProjectRequest)
  let name = projectAddProjectRequestName projectAddProjectRequest
  let description = projectAddProjectRequestDescription projectAddProjectRequest
  let uuid = Data.UUID.V4.nextRandom
  let uuidText = unsafePerformIO $ liftM Data.UUID.toText uuid
  let project = Project name uid description uuidText
  insertedProject <- runDB $ insertEntity project
  returnJson insertedProject
        
       
postProjectListGetProjectsR :: Handler Value     
postProjectListGetProjectsR = do     
  _ <- (requireJsonBody :: Handler HITEDModule.Utilities.NullRequest)
  (uid,_) <- requireAuthPair
  allProjects <-   getAllProjectsForUser uid
  allProjectInWhichUserOnlyParticipates <- getAllProjectsInWhichUserOnlyParticipates uid
  -- define the local function transform that construct AugmentedProjectData based on
  -- the Entity Project and sets the augmentedProjectDataIsOwner flag to True if
  -- the owner is the same as the user  
  let transform = \ep -> AugmentedProjectData {
        augmentedProjectDataEntityProject = ep
        , augmentedProjectDataIsOwner = case ep of
            Entity _ projectRecord -> uid == (projectOwner projectRecord)
        }
  -- apply transform to the merge of both results
  let result = fmap transform (allProjects ++ allProjectInWhichUserOnlyParticipates)
  returnJson result
 
postProjectGetProjectsInWhichUserOnlyParticipatesR :: Handler Value
postProjectGetProjectsInWhichUserOnlyParticipatesR = do
  _ <- (requireJsonBody :: Handler HITEDModule.Utilities.NullRequest)
  (uid, _) <- requireAuthPair
  allProjectInWhichUserOnlyParticipates <- getAllProjectsInWhichUserOnlyParticipates uid
  returnJson allProjectInWhichUserOnlyParticipates
 
-- portProjectListRemoveProjectsR
-- Purpose: REST handle for removing projects
-- Input
--   Yesod default 
-- Returns
--   Handler Value: HTTP code 200 if success
--   Handler Value: HTTP code 403 if CSRF failure
-- Exceptions     
--   All Persist exceptions    

postProjectListRemoveProjectsR :: Handler Value 
postProjectListRemoveProjectsR = do
  proceed <- hasValidCsrfHeaderNamed $ mk $ defaultCsrfCookieName
  case proceed of
    True -> do
      removeProjectList <- (requireJsonBody :: Handler ProjectRemoveProjectsRequest)
      maybeCurrentUserId <- maybeAuthId
      _ <- removeProjects (projectRemoveProjectsRequestListOfProjects removeProjectList) maybeCurrentUserId
      sendResponseStatus status200 ("Success"::Text)
    False -> do
      sendResponseStatus status403 ("CSRF Failure"::Text)

postProjectGetAssociatedTeamsR :: Handler Value
postProjectGetAssociatedTeamsR = do
  getAssociatedTeamsRequest <- (requireJsonBody :: Handler ProjectGetAssociatedTeamsRequest)
  let projectId = projectGetAssociatedTeamsRequestProjectId getAssociatedTeamsRequest
  let rowId = projectGetAssociatedTeamsRequestRowId getAssociatedTeamsRequest
  result <- getAssociatedTeams projectId
  let response = ProjectGetAssociatedTeamsResponse {
        projectGetAssociatedTeamsResponseEntityTeamData = result
        , projectGetAssociatedTeamsResponseRowId = rowId
        }
  returnJson response

postProjectAddTeamToProjectR :: Handler Value
postProjectAddTeamToProjectR = do
  projectAddTeamToProjectRequest <- (requireJsonBody :: Handler ProjectAddTeamToProjectRequest)
  let teamIds = projectAddTeamToProjectRequestTeamIds projectAddTeamToProjectRequest
  let projectUuid = projectAddTeamToProjectRequestProjectUuid projectAddTeamToProjectRequest
  _ <- addTeamToProject teamIds projectUuid
  let result = ProjectAddTeamToProjectResponse True
  returnJson result
 
postProjectRemoveTeamFromProjectR :: Handler Value
postProjectRemoveTeamFromProjectR = do
  projectRemoveTeamFromProjectRequest <- (requireJsonBody :: Handler ProjectRemoveTeamFromProjectRequest)
  let teamIds = projectRemoveTeamFromProjectRequestTeamIds projectRemoveTeamFromProjectRequest
  let projectUuid = projectRemoveTeamFromProjectRequestProjectUuid projectRemoveTeamFromProjectRequest
  _ <- removeTeamFromProject teamIds projectUuid
  let result = ProjectRemoveTeamFromProjectResponse True
  returnJson result


postProjectUpdateProjectR :: Handler Value
postProjectUpdateProjectR = do
  updateProjectRequest <- (requireJsonBody :: Handler ProjectUpdateProjectRequest)
  let projectUuid = projectUpdateProjectRequestProjectUuid updateProjectRequest
  let projectName = projectUpdateProjectRequestProjectName updateProjectRequest
  let projectDescription = projectUpdateProjectRequestProjectDescription updateProjectRequest
  _ <- updateProject projectUuid projectName projectDescription
  (uid,_) <- requireAuthPair
  allProjects <- getAllProjectsForUser uid
  allProjectInWhichUserOnlyParticipates <- getAllProjectsInWhichUserOnlyParticipates uid
  -- define the local function transform that construct AugmentedProjectData based on
  -- the Entity Project and sets the augmentedProjectDataIsOwner flag to True if
  -- the owner is the same as the user  
  let transform = \ep -> AugmentedProjectData {
        augmentedProjectDataEntityProject = ep
        , augmentedProjectDataIsOwner = case ep of
            Entity _ projectRecord -> uid == (projectOwner projectRecord)
        }
  -- apply transform to the merge of both results
  let result = fmap transform (allProjects ++ allProjectInWhichUserOnlyParticipates)
  returnJson result


postProjectGetNotInProjectTeamsR :: Handler Value
postProjectGetNotInProjectTeamsR = do
  (uid,_) <- requireAuthPair
  getNotInProjectTeamsRequest <- (requireJsonBody :: Handler ProjectGetNotInProjectTeamsRequest)
  let projectUuid = projectGetNotInProjectTeamsRequestProjectUuid getNotInProjectTeamsRequest
  teamData <-  projectGetNotInProjectTeams projectUuid uid
  let result = ProjectGetNotInProjectTeamsResponse {
        projectGetNotInProjectTeamsResponseTeamData = teamData,
        projectGetNotInProjectTeamsResponseRowIndex = projectGetNotInProjectTeamsRequestRowIndex  getNotInProjectTeamsRequest
        }
  returnJson result


postProjectGetInProjectTeamsR :: Handler Value
postProjectGetInProjectTeamsR = do
  (uid,_) <- requireAuthPair
  getInProjectTeamsRequest <- (requireJsonBody :: Handler ProjectGetInProjectTeamsRequest)
  teamData <-  projectGetInProjectTeams (projectGetInProjectTeamsRequestProjectUuid getInProjectTeamsRequest) uid
  let result = ProjectGetInProjectTeamsResponse {
        projectGetInProjectTeamsResponseTeamData = teamData,
        projectGetInProjectTeamsResponseRowIndex = projectGetInProjectTeamsRequestRowIndex getInProjectTeamsRequest}
  returnJson result

-- Support functions in project page 
      

-- getAllProjects
-- Purpose: obtain all projects available in the database and return a list
-- Input
--  N/A
-- Returns
--  [Entity Project]
-- Exceptions
--  All Persist exceptions
 
getAllProjects :: Handler [Entity Project]
getAllProjects = do
  runDB $ selectList [] [Asc ProjectName]

-- getAllProjectsForUser
-- Purpose: obtain all projects available to the logged in user
-- Input
--  maybeCurrentUserId: Maybe UserId: If <> Nothing, then the user identity
-- Returns
--  [Entity Project]: A list of projects
-- Exceptions
--  All Persist exceptions


getAllProjectsForUser :: UserId -> Handler [Entity Project]
getAllProjectsForUser userId = do
  runDB $ rawSql s [toPersistValue userId]
    where s = "SELECT ?? FROM project WHERE owner = ? ORDER BY name ASC"


getAllProjectsInWhichUserOnlyParticipates :: UserId -> Handler [Entity Project]
getAllProjectsInWhichUserOnlyParticipates uid = do
  runDB $ rawSql q [toPersistValue uid, toPersistValue uid] where
    q = "SELECT ?? FROM project WHERE project.id IN (SELECT project_id FROM project_team_relation WHERE team_id IN (SELECT team_id FROM team_member WHERE user_id = ?)) AND project.owner <> ? ORDER BY project.name ASC"


  
-- removeProjects
-- Purpose: remove all projects owned by the user
-- Input
--   listOfProjects: [String]: List of names of projects
--   maybeCurrentUserId: Maybe UserId: If <> Nothing, then the user identity
-- Returns
--   ()
-- Exceptions
--   All Persist exceptions
 

removeProjects :: [Data.UUID.UUID] -> Maybe UserId -> Handler ()
removeProjects listOfProjects maybeCurrentUserId = do
  let listOfProjects' = fmap Data.UUID.toText listOfProjects

  case maybeCurrentUserId of
    Just userId -> do
      let removeProjects' = \lop ->
            case lop of
              x:xs -> do
                runDB $ [executeQQ|
                                  DELETE FROM ^{Project} WHERE @{ProjectOwner} = #{userId} AND @{ProjectUuid} =  #{x}
                                  |]
                removeProjects' xs
              [] -> do
                return ()
        
      removeProjects' listOfProjects'
    Nothing -> do
      return ()

 
getAssociatedTeams :: ProjectId -> Handler [Entity Team]
getAssociatedTeams projectId = do
  runDB $ rawSql s [toPersistValue projectId] where
    s = "SELECT ?? FROM team WHERE id IN (SELECT team_id FROM project_team_relation WHERE project_id = ?)"


addTeamToProject :: [TeamId] -> Data.UUID.UUID -> Handler ()
addTeamToProject teamIds projectUuid = do
  let projectUuidText = Data.UUID.toText projectUuid
  maybeEntityProject <- runDB $ selectFirst[ProjectUuid ==. projectUuidText][]
  case maybeEntityProject of
    Just (Entity projectId _) -> do
      let addTeamToProject' = \teamId ->
            case teamId of
              t:ts -> do
                _ <- runDB $ insert (ProjectTeamRelation projectId t)
                addTeamToProject' ts
              [] -> do
                return ()
      addTeamToProject' teamIds
    Nothing -> do
      return ()

removeTeamFromProject :: [TeamId] -> Data.UUID.UUID -> Handler ()
removeTeamFromProject teamIds projectUuid = do
  let projectUuidText = Data.UUID.toText projectUuid
  maybeEntityProject <- runDB $ selectFirst[ProjectUuid ==. projectUuidText][]
  case maybeEntityProject of
    Just (Entity projectId _) -> do
      let removeTeamFromProject' = \teamId ->
            case teamId of
              t:ts -> do
                _ <- runDB $ deleteWhere[ProjectTeamRelationTeamId ==. t, ProjectTeamRelationProjectId ==. projectId]
                removeTeamFromProject' ts
              [] -> do
                return ()
      removeTeamFromProject' teamIds
    Nothing -> do
      return ()

projectGetNotInProjectTeams :: Data.UUID.UUID -> UserId -> Handler [Entity Team]
projectGetNotInProjectTeams projectUuid owner = do
  let projectUuidText = Data.UUID.toText projectUuid
  runDB $ rawSql q [toPersistValue projectUuidText,toPersistValue owner] where
    q = "SELECT ?? FROM team WHERE team.id NOT IN (SELECT team_id FROM project_team_relation WHERE project_team_relation.project_id IN (SELECT project.id FROM project WHERE project.uuid = ?)) AND team.owner = ? ORDER BY team.name";

 
projectGetInProjectTeams :: Data.UUID.UUID -> UserId -> Handler [Entity Team]
projectGetInProjectTeams projectUuid owner = do
  let projectUuidText = Data.UUID.toText projectUuid
  runDB $ rawSql q [toPersistValue projectUuidText,toPersistValue owner] where
    q = "SELECT ?? FROM team WHERE team.id IN (SELECT team_id FROM project_team_relation WHERE project_team_relation.project_id  IN (SELECT project.id FROM project WHERE project.uuid = ?)) AND team.owner = ? ORDER BY team.name";

updateProject :: Data.UUID.UUID -> Text -> Text -> Handler ()
updateProject projectUuid projectName projectDescription = do
  _ <- runDB $ [executeQQ|
                         UPDATE ^{Project}
                         SET @{ProjectName} = #{projectName},
                         @{ProjectDescription} = #{projectDescription}
                         WHERE @{ProjectUuid} = #{Data.UUID.toText projectUuid}
                         |]
  return ()



