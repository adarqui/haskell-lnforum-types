{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Convert where




import           Control.DeepSeq             (NFData)
import           Data.Aeson                  (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Default                (Default, def)
import           Data.Int                    (Int64)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time                   (UTCTime)
import           Data.Typeable               (Typeable)
import           Data.Monoid                 ((<>))
import           GHC.Generics                (Generic)
import           Haskell.Api.Helpers.Shared  (QueryParam, qp)

import LN.T

apiRequestToApiResponse :: Int64 -> Int64 -> Text -> (Maybe UTCTime) -> (Maybe UTCTime) -> ApiRequest -> ApiResponse
apiRequestToApiResponse _1 _2 _3 _4 _5 ApiRequest{..} =
  ApiResponse {
    apiResponseId = _1,
    apiResponseUserId = _2,
    apiResponseKey = _3,
    apiResponseCreatedAt = _4,
    apiResponseModifiedAt = _5,
    apiResponseComment = apiRequestComment,
    apiResponseGuard = apiRequestGuard
  }


apiResponseToApiRequest :: ApiResponse -> ApiRequest
apiResponseToApiRequest  ApiResponse{..} =
  ApiRequest {
    apiRequestComment = apiResponseComment,
    apiRequestGuard = apiResponseGuard
  }


boardRequestToBoardResponse :: Int64 -> Int64 -> Int64 -> Int64 -> (Maybe Int64) -> Text -> Bool -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> BoardRequest -> BoardResponse
boardRequestToBoardResponse _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 BoardRequest{..} =
  BoardResponse {
    boardResponseId = _1,
    boardResponseUserId = _2,
    boardResponseOrgId = _3,
    boardResponseForumId = _4,
    boardResponseParentId = _5,
    boardResponseName = _6,
    boardResponseActive = _7,
    boardResponseCreatedAt = _8,
    boardResponseModifiedBy = _9,
    boardResponseModifiedAt = _10,
    boardResponseActivityAt = _11,
    boardResponseDisplayName = boardRequestDisplayName,
    boardResponseDescription = boardRequestDescription,
    boardResponseIsAnonymous = boardRequestIsAnonymous,
    boardResponseCanCreateSubBoards = boardRequestCanCreateSubBoards,
    boardResponseCanCreateThreads = boardRequestCanCreateThreads,
    boardResponseSuggestedTags = boardRequestSuggestedTags,
    boardResponseIcon = boardRequestIcon,
    boardResponseTags = boardRequestTags,
    boardResponseGuard = boardRequestGuard
  }


boardResponseToBoardRequest :: BoardResponse -> BoardRequest
boardResponseToBoardRequest  BoardResponse{..} =
  BoardRequest {
    boardRequestDisplayName = boardResponseDisplayName,
    boardRequestDescription = boardResponseDescription,
    boardRequestIsAnonymous = boardResponseIsAnonymous,
    boardRequestCanCreateSubBoards = boardResponseCanCreateSubBoards,
    boardRequestCanCreateThreads = boardResponseCanCreateThreads,
    boardRequestSuggestedTags = boardResponseSuggestedTags,
    boardRequestIcon = boardResponseIcon,
    boardRequestTags = boardResponseTags,
    boardRequestGuard = boardResponseGuard
  }


bucketRequestToBucketResponse :: Int64 -> Int64 -> Text -> Bool -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> BucketRequest -> BucketResponse
bucketRequestToBucketResponse _1 _2 _3 _4 _5 _6 _7 BucketRequest{..} =
  BucketResponse {
    bucketResponseId = _1,
    bucketResponseUserId = _2,
    bucketResponseName = _3,
    bucketResponseActive = _4,
    bucketResponseCreatedAt = _5,
    bucketResponseModifiedAt = _6,
    bucketResponseActivityAt = _7,
    bucketResponseDisplayName = bucketRequestDisplayName,
    bucketResponseDescription = bucketRequestDescription,
    bucketResponseScoreLo = bucketRequestScoreLo,
    bucketResponseScoreHi = bucketRequestScoreHi,
    bucketResponseLeurons = bucketRequestLeurons,
    bucketResponseResources = bucketRequestResources,
    bucketResponseCategories = bucketRequestCategories,
    bucketResponseFilters = bucketRequestFilters,
    bucketResponseGuard = bucketRequestGuard
  }


bucketResponseToBucketRequest :: BucketResponse -> BucketRequest
bucketResponseToBucketRequest  BucketResponse{..} =
  BucketRequest {
    bucketRequestDisplayName = bucketResponseDisplayName,
    bucketRequestDescription = bucketResponseDescription,
    bucketRequestScoreLo = bucketResponseScoreLo,
    bucketRequestScoreHi = bucketResponseScoreHi,
    bucketRequestLeurons = bucketResponseLeurons,
    bucketRequestResources = bucketResponseResources,
    bucketRequestCategories = bucketResponseCategories,
    bucketRequestFilters = bucketResponseFilters,
    bucketRequestGuard = bucketResponseGuard
  }


forumRequestToForumResponse :: Int64 -> Int64 -> Int64 -> Text -> Bool -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ForumRequest -> ForumResponse
forumRequestToForumResponse _1 _2 _3 _4 _5 _6 _7 _8 _9 ForumRequest{..} =
  ForumResponse {
    forumResponseId = _1,
    forumResponseUserId = _2,
    forumResponseOrgId = _3,
    forumResponseName = _4,
    forumResponseActive = _5,
    forumResponseCreatedAt = _6,
    forumResponseModifiedBy = _7,
    forumResponseModifiedAt = _8,
    forumResponseActivityAt = _9,
    forumResponseDisplayName = forumRequestDisplayName,
    forumResponseDescription = forumRequestDescription,
    forumResponseThreadsPerBoard = forumRequestThreadsPerBoard,
    forumResponseThreadPostsPerThread = forumRequestThreadPostsPerThread,
    forumResponseRecentThreadsLimit = forumRequestRecentThreadsLimit,
    forumResponseRecentPostsLimit = forumRequestRecentPostsLimit,
    forumResponseMotwLimit = forumRequestMotwLimit,
    forumResponseIcon = forumRequestIcon,
    forumResponseTags = forumRequestTags,
    forumResponseVisibility = forumRequestVisibility,
    forumResponseGuard = forumRequestGuard
  }


forumResponseToForumRequest :: ForumResponse -> ForumRequest
forumResponseToForumRequest  ForumResponse{..} =
  ForumRequest {
    forumRequestDisplayName = forumResponseDisplayName,
    forumRequestDescription = forumResponseDescription,
    forumRequestThreadsPerBoard = forumResponseThreadsPerBoard,
    forumRequestThreadPostsPerThread = forumResponseThreadPostsPerThread,
    forumRequestRecentThreadsLimit = forumResponseRecentThreadsLimit,
    forumRequestRecentPostsLimit = forumResponseRecentPostsLimit,
    forumRequestMotwLimit = forumResponseMotwLimit,
    forumRequestIcon = forumResponseIcon,
    forumRequestTags = forumResponseTags,
    forumRequestVisibility = forumResponseVisibility,
    forumRequestGuard = forumResponseGuard
  }


leuronRequestToLeuronResponse :: Int64 -> Int64 -> Int64 -> Bool -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> LeuronRequest -> LeuronResponse
leuronRequestToLeuronResponse _1 _2 _3 _4 _5 _6 _7 LeuronRequest{..} =
  LeuronResponse {
    leuronResponseId = _1,
    leuronResponseUserId = _2,
    leuronResponseResourceId = _3,
    leuronResponseActive = _4,
    leuronResponseCreatedAt = _5,
    leuronResponseModifiedAt = _6,
    leuronResponseActivityAt = _7,
    leuronResponseData = leuronRequestData,
    leuronResponseTitle = leuronRequestTitle,
    leuronResponseDescription = leuronRequestDescription,
    leuronResponseSection = leuronRequestSection,
    leuronResponsePage = leuronRequestPage,
    leuronResponseExamples = leuronRequestExamples,
    leuronResponseStrengths = leuronRequestStrengths,
    leuronResponseCategories = leuronRequestCategories,
    leuronResponseSplits = leuronRequestSplits,
    leuronResponseSubstitutions = leuronRequestSubstitutions,
    leuronResponseTags = leuronRequestTags,
    leuronResponseStyle = leuronRequestStyle,
    leuronResponseGuard = leuronRequestGuard
  }


leuronResponseToLeuronRequest :: LeuronResponse -> LeuronRequest
leuronResponseToLeuronRequest  LeuronResponse{..} =
  LeuronRequest {
    leuronRequestData = leuronResponseData,
    leuronRequestTitle = leuronResponseTitle,
    leuronRequestDescription = leuronResponseDescription,
    leuronRequestSection = leuronResponseSection,
    leuronRequestPage = leuronResponsePage,
    leuronRequestExamples = leuronResponseExamples,
    leuronRequestStrengths = leuronResponseStrengths,
    leuronRequestCategories = leuronResponseCategories,
    leuronRequestSplits = leuronResponseSplits,
    leuronRequestSubstitutions = leuronResponseSubstitutions,
    leuronRequestTags = leuronResponseTags,
    leuronRequestStyle = leuronResponseStyle,
    leuronRequestGuard = leuronResponseGuard
  }


leuronTrainingRequestToLeuronTrainingResponse :: Int64 -> Int64 -> Int64 -> (Maybe UTCTime) -> (Maybe UTCTime) -> LeuronTrainingRequest -> LeuronTrainingResponse
leuronTrainingRequestToLeuronTrainingResponse _1 _2 _3 _4 _5 LeuronTrainingRequest{..} =
  LeuronTrainingResponse {
    leuronTrainingResponseId = _1,
    leuronTrainingResponseUserId = _2,
    leuronTrainingResponseLeuronId = _3,
    leuronTrainingResponseCreatedAt = _4,
    leuronTrainingResponseModifiedAt = _5,
    leuronTrainingResponseSummary = leuronTrainingRequestSummary,
    leuronTrainingResponseGuard = leuronTrainingRequestGuard
  }


leuronTrainingResponseToLeuronTrainingRequest :: LeuronTrainingResponse -> LeuronTrainingRequest
leuronTrainingResponseToLeuronTrainingRequest  LeuronTrainingResponse{..} =
  LeuronTrainingRequest {
    leuronTrainingRequestSummary = leuronTrainingResponseSummary,
    leuronTrainingRequestGuard = leuronTrainingResponseGuard
  }


likeRequestToLikeResponse :: Int64 -> Ent -> Int64 -> Int64 -> Int -> Bool -> (Maybe UTCTime) -> (Maybe UTCTime) -> LikeRequest -> LikeResponse
likeRequestToLikeResponse _1 _2 _3 _4 _5 _6 _7 _8 LikeRequest{..} =
  LikeResponse {
    likeResponseId = _1,
    likeResponseEnt = _2,
    likeResponseEntId = _3,
    likeResponseUserId = _4,
    likeResponseScore = _5,
    likeResponseActive = _6,
    likeResponseCreatedAt = _7,
    likeResponseModifiedAt = _8,
    likeResponseOpt = likeRequestOpt,
    likeResponseReason = likeRequestReason,
    likeResponseGuard = likeRequestGuard
  }


likeResponseToLikeRequest :: LikeResponse -> LikeRequest
likeResponseToLikeRequest  LikeResponse{..} =
  LikeRequest {
    likeRequestOpt = likeResponseOpt,
    likeRequestReason = likeResponseReason,
    likeRequestGuard = likeResponseGuard
  }


organizationRequestToOrganizationResponse :: Int64 -> Int64 -> Text -> Text -> Bool -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> OrganizationRequest -> OrganizationResponse
organizationRequestToOrganizationResponse _1 _2 _3 _4 _5 _6 _7 _8 _9 OrganizationRequest{..} =
  OrganizationResponse {
    organizationResponseId = _1,
    organizationResponseUserId = _2,
    organizationResponseName = _3,
    organizationResponseEmailMD5 = _4,
    organizationResponseActive = _5,
    organizationResponseCreatedAt = _6,
    organizationResponseModifiedBy = _7,
    organizationResponseModifiedAt = _8,
    organizationResponseActivityAt = _9,
    organizationResponseDisplayName = organizationRequestDisplayName,
    organizationResponseDescription = organizationRequestDescription,
    organizationResponseCompany = organizationRequestCompany,
    organizationResponseLocation = organizationRequestLocation,
    organizationResponseEmail = organizationRequestEmail,
    organizationResponseMembership = organizationRequestMembership,
    organizationResponseTags = organizationRequestTags,
    organizationResponseIcon = organizationRequestIcon,
    organizationResponseVisibility = organizationRequestVisibility,
    organizationResponseGuard = organizationRequestGuard
  }


organizationResponseToOrganizationRequest :: OrganizationResponse -> OrganizationRequest
organizationResponseToOrganizationRequest  OrganizationResponse{..} =
  OrganizationRequest {
    organizationRequestDisplayName = organizationResponseDisplayName,
    organizationRequestDescription = organizationResponseDescription,
    organizationRequestCompany = organizationResponseCompany,
    organizationRequestLocation = organizationResponseLocation,
    organizationRequestEmail = organizationResponseEmail,
    organizationRequestMembership = organizationResponseMembership,
    organizationRequestIcon = organizationResponseIcon,
    organizationRequestTags = organizationResponseTags,
    organizationRequestVisibility = organizationResponseVisibility,
    organizationRequestGuard = organizationResponseGuard
  }


pmRequestToPmResponse :: Int64 -> Int64 -> Int64 -> Bool -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> PmRequest -> PmResponse
pmRequestToPmResponse _1 _2 _3 _4 _5 _6 _7 PmRequest{..} =
  PmResponse {
    pmResponseId = _1,
    pmResponseUserId = _2,
    pmResponseToUserId = _3,
    pmResponseActive = _4,
    pmResponseCreatedAt = _5,
    pmResponseModifiedAt = _6,
    pmResponseActivityAt = _7,
    pmResponseSubject = pmRequestSubject,
    pmResponseBody = pmRequestBody,
    pmResponseGuard = pmRequestGuard
  }


pmResponseToPmRequest :: PmResponse -> PmRequest
pmResponseToPmRequest  PmResponse{..} =
  PmRequest {
    pmRequestSubject = pmResponseSubject,
    pmRequestBody = pmResponseBody,
    pmRequestGuard = pmResponseGuard
  }


pmInRequestToPmInResponse :: Int64 -> Int64 -> Int64 -> Bool -> Bool -> Bool -> (Maybe UTCTime) -> (Maybe UTCTime) -> PmInRequest -> PmInResponse
pmInRequestToPmInResponse _1 _2 _3 _4 _5 _6 _7 _8 PmInRequest{..} =
  PmInResponse {
    pmInResponseId = _1,
    pmInResponsePmId = _2,
    pmInResponseUserId = _3,
    pmInResponseIsNew = _4,
    pmInResponseIsSaved = _5,
    pmInResponseActive = _6,
    pmInResponseCreatedAt = _7,
    pmInResponseModifiedAt = _8,
    pmInResponseLabel = pmInRequestLabel,
    pmInResponseIsRead = pmInRequestIsRead,
    pmInResponseIsStarred = pmInRequestIsStarred,
    pmInResponseGuard = pmInRequestGuard
  }


pmInResponseToPmInRequest :: PmInResponse -> PmInRequest
pmInResponseToPmInRequest  PmInResponse{..} =
  PmInRequest {
    pmInRequestLabel = pmInResponseLabel,
    pmInRequestIsRead = pmInResponseIsRead,
    pmInRequestIsStarred = pmInResponseIsStarred,
    pmInRequestGuard = pmInResponseGuard
  }


pmOutRequestToPmOutResponse :: Int64 -> Int64 -> Int64 -> Bool -> Bool -> (Maybe UTCTime) -> (Maybe UTCTime) -> PmOutRequest -> PmOutResponse
pmOutRequestToPmOutResponse _1 _2 _3 _4 _5 _6 _7 PmOutRequest{..} =
  PmOutResponse {
    pmOutResponseId = _1,
    pmOutResponsePmId = _2,
    pmOutResponseUserId = _3,
    pmOutResponseIsSaved = _4,
    pmOutResponseActive = _5,
    pmOutResponseCreatedAt = _6,
    pmOutResponseModifiedAt = _7,
    pmOutResponseLabel = pmOutRequestLabel,
    pmOutResponseGuard = pmOutRequestGuard
  }


pmOutResponseToPmOutRequest :: PmOutResponse -> PmOutRequest
pmOutResponseToPmOutRequest  PmOutResponse{..} =
  PmOutRequest {
    pmOutRequestLabel = pmOutResponseLabel,
    pmOutRequestGuard = pmOutResponseGuard
  }


profileRequestToProfileResponse :: Int64 -> Ent -> Int64 -> Int -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> ProfileRequest -> ProfileResponse
profileRequestToProfileResponse _1 _2 _3 _4 _5 _6 _7 ProfileRequest{..} =
  ProfileResponse {
    profileResponseId = _1,
    profileResponseEnt = _2,
    profileResponseEntId = _3,
    profileResponseKarmaGood = _4,
    profileResponseKarmaBad = _5,
    profileResponseCreatedAt = _6,
    profileResponseModifiedAt = _7,
    profileResponseGender = profileRequestGender,
    profileResponseBirthdate = profileRequestBirthdate,
    profileResponseWebsite = profileRequestWebsite,
    profileResponseLocation = profileRequestLocation,
    profileResponseSignature = profileRequestSignature,
    profileResponseDebug = profileRequestDebug,
    profileResponseGuard = profileRequestGuard
  }


profileResponseToProfileRequest :: ProfileResponse -> ProfileRequest
profileResponseToProfileRequest  ProfileResponse{..} =
  ProfileRequest {
    profileRequestGender = profileResponseGender,
    profileRequestBirthdate = profileResponseBirthdate,
    profileRequestWebsite = profileResponseWebsite,
    profileRequestLocation = profileResponseLocation,
    profileRequestSignature = profileResponseSignature,
    profileRequestDebug = profileResponseDebug,
    profileRequestGuard = profileResponseGuard
  }


reminderRequestToReminderResponse :: Int64 -> Int64 -> Int64 -> Bool -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ReminderRequest -> ReminderResponse
reminderRequestToReminderResponse _1 _2 _3 _4 _5 _6 _7 ReminderRequest{..} =
  ReminderResponse {
    reminderResponseId = _1,
    reminderResponseUserId = _2,
    reminderResponseParentFolderId = _3,
    reminderResponseActive = _4,
    reminderResponseCreatedAt = _5,
    reminderResponseModifiedAt = _6,
    reminderResponseActivityAt = _7,
    reminderResponseData = reminderRequestData,
    reminderResponseGuard = reminderRequestGuard
  }


reminderResponseToReminderRequest :: ReminderResponse -> ReminderRequest
reminderResponseToReminderRequest  ReminderResponse{..} =
  ReminderRequest {
    reminderRequestData = reminderResponseData,
    reminderRequestGuard = reminderResponseGuard
  }


reminderFolderRequestToReminderFolderResponse :: Int64 -> Int64 -> (Maybe Int64) -> Text -> Bool -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ReminderFolderRequest -> ReminderFolderResponse
reminderFolderRequestToReminderFolderResponse _1 _2 _3 _4 _5 _6 _7 _8 ReminderFolderRequest{..} =
  ReminderFolderResponse {
    reminderFolderResponseId = _1,
    reminderFolderResponseUserId = _2,
    reminderFolderResponseParentFolderId = _3,
    reminderFolderResponseName = _4,
    reminderFolderResponseActive = _5,
    reminderFolderResponseCreatedAt = _6,
    reminderFolderResponseModifiedAt = _7,
    reminderFolderResponseActivityAt = _8,
    reminderFolderResponseDisplayName = reminderFolderRequestDisplayName,
    reminderFolderResponseDescription = reminderFolderRequestDescription,
    reminderFolderResponseVisibility = reminderFolderRequestVisibility,
    reminderFolderResponseGuard = reminderFolderRequestGuard
  }


reminderFolderResponseToReminderFolderRequest :: ReminderFolderResponse -> ReminderFolderRequest
reminderFolderResponseToReminderFolderRequest  ReminderFolderResponse{..} =
  ReminderFolderRequest {
    reminderFolderRequestDisplayName = reminderFolderResponseDisplayName,
    reminderFolderRequestVisibility = reminderFolderResponseVisibility,
    reminderFolderRequestDescription = reminderFolderResponseDescription,
    reminderFolderRequestGuard = reminderFolderResponseGuard
  }


resourceRequestToResourceResponse :: Int64 -> Int64 -> Text -> Bool -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ResourceRequest -> ResourceResponse
resourceRequestToResourceResponse _1 _2 _3 _4 _5 _6 _7 ResourceRequest{..} =
  ResourceResponse {
    resourceResponseId = _1,
    resourceResponseUserId = _2,
    resourceResponseName = _3,
    resourceResponseActive = _4,
    resourceResponseCreatedAt = _5,
    resourceResponseModifiedAt = _6,
    resourceResponseActivityAt = _7,
    resourceResponseDisplayName = resourceRequestDisplayName,
    resourceResponseDescription = resourceRequestDescription,
    resourceResponseSource = resourceRequestSource,
    resourceResponseAuthor = resourceRequestAuthor,
    resourceResponsePrerequisites = resourceRequestPrerequisites,
    resourceResponseCategories = resourceRequestCategories,
    resourceResponseVisibility = resourceRequestVisibility,
    resourceResponseCounter = resourceRequestCounter,
    resourceResponseVersion = resourceRequestVersion,
    resourceResponseUrls = resourceRequestUrls,
    resourceResponseIcon = resourceRequestIcon,
    resourceResponseTags = resourceRequestTags,
    resourceResponseGuard = resourceRequestGuard
  }


resourceResponseToResourceRequest :: ResourceResponse -> ResourceRequest
resourceResponseToResourceRequest  ResourceResponse{..} =
  ResourceRequest {
    resourceRequestDisplayName = resourceResponseDisplayName,
    resourceRequestDescription = resourceResponseDescription,
    resourceRequestSource = resourceResponseSource,
    resourceRequestAuthor = resourceResponseAuthor,
    resourceRequestPrerequisites = resourceResponsePrerequisites,
    resourceRequestCategories = resourceResponseCategories,
    resourceRequestVisibility = resourceResponseVisibility,
    resourceRequestCounter = resourceResponseCounter,
    resourceRequestVersion = resourceResponseVersion,
    resourceRequestUrls = resourceResponseUrls,
    resourceRequestIcon = resourceResponseIcon,
    resourceRequestTags = resourceResponseTags,
    resourceRequestGuard = resourceResponseGuard
  }


starRequestToStarResponse :: Int64 -> Ent -> Int64 -> Int64 -> Bool -> (Maybe UTCTime) -> (Maybe UTCTime) -> StarRequest -> StarResponse
starRequestToStarResponse _1 _2 _3 _4 _5 _6 _7 StarRequest{..} =
  StarResponse {
    starResponseId = _1,
    starResponseEnt = _2,
    starResponseEntId = _3,
    starResponseUserId = _4,
    starResponseActive = _5,
    starResponseCreatedAt = _6,
    starResponseModifiedAt = _7,
    starResponseReason = starRequestReason,
    starResponseGuard = starRequestGuard
  }


starResponseToStarRequest :: StarResponse -> StarRequest
starResponseToStarRequest  StarResponse{..} =
  StarRequest {
    starRequestReason = starResponseReason,
    starRequestGuard = starResponseGuard
  }


teamRequestToTeamResponse :: Int64 -> Int64 -> Int64 -> SystemTeam -> Bool -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> TeamRequest -> TeamResponse
teamRequestToTeamResponse _1 _2 _3 _4 _5 _6 _7 _8 _9 TeamRequest{..} =
  TeamResponse {
    teamResponseId = _1,
    teamResponseUserId = _2,
    teamResponseOrgId = _3,
    teamResponseSystem = _4,
    teamResponseActive = _5,
    teamResponseCreatedAt = _6,
    teamResponseModifiedBy = _7,
    teamResponseModifiedAt = _8,
    teamResponseActivityAt = _9,
    teamResponseMembership = teamRequestMembership,
    teamResponseIcon = teamRequestIcon,
    teamResponseTags = teamRequestTags,
    teamResponseVisibility = teamRequestVisibility,
    teamResponseGuard = teamRequestGuard
  }


teamResponseToTeamRequest :: TeamResponse -> TeamRequest
teamResponseToTeamRequest  TeamResponse{..} =
  TeamRequest {
    teamRequestMembership = teamResponseMembership,
    teamRequestIcon = teamResponseIcon,
    teamRequestTags = teamResponseTags,
    teamRequestVisibility = teamResponseVisibility,
    teamRequestGuard = teamResponseGuard
  }


threadRequestToThreadResponse :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Text -> Bool -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ThreadRequest -> ThreadResponse
threadRequestToThreadResponse _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 ThreadRequest{..} =
  ThreadResponse {
    threadResponseId = _1,
    threadResponseUserId = _2,
    threadResponseOrgId = _3,
    threadResponseForumId = _4,
    threadResponseBoardId = _5,
    threadResponseName = _6,
    threadResponseActive = _7,
    threadResponseCreatedAt = _8,
    threadResponseModifiedBy = _9,
    threadResponseModifiedAt = _10,
    threadResponseActivityAt = _11,
    threadResponseDisplayName = threadRequestDisplayName,
    threadResponseDescription = threadRequestDescription,
    threadResponseSticky = threadRequestSticky,
    threadResponseLocked = threadRequestLocked,
    threadResponsePoll = threadRequestPoll,
    threadResponseIcon = threadRequestIcon,
    threadResponseTags = threadRequestTags,
    threadResponseGuard = threadRequestGuard
  }


threadResponseToThreadRequest :: ThreadResponse -> ThreadRequest
threadResponseToThreadRequest  ThreadResponse{..} =
  ThreadRequest {
    threadRequestDisplayName = threadResponseDisplayName,
    threadRequestDescription = threadResponseDescription,
    threadRequestSticky = threadResponseSticky,
    threadRequestLocked = threadResponseLocked,
    threadRequestPoll = threadResponsePoll,
    threadRequestIcon = threadResponseIcon,
    threadRequestTags = threadResponseTags,
    threadRequestGuard = threadResponseGuard
  }


threadPostRequestToThreadPostResponse :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> (Maybe Int64) -> Bool -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ThreadPostRequest -> ThreadPostResponse
threadPostRequestToThreadPostResponse _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 ThreadPostRequest{..} =
  ThreadPostResponse {
    threadPostResponseId = _1,
    threadPostResponseUserId = _2,
    threadPostResponseOrgId = _3,
    threadPostResponseForumId = _4,
    threadPostResponseBoardId = _5,
    threadPostResponseThreadId = _6,
    threadPostResponseParentId = _7,
    threadPostResponseActive = _8,
    threadPostResponseCreatedAt = _9,
    threadPostResponseModifiedBy = _10,
    threadPostResponseModifiedAt = _11,
    threadPostResponseActivityAt = _12,
    threadPostResponseTitle = threadPostRequestTitle,
    threadPostResponseBody = threadPostRequestBody,
    threadPostResponseTags = threadPostRequestTags,
    threadPostResponsePrivateTags = threadPostRequestPrivateTags,
    threadPostResponseGuard = threadPostRequestGuard
  }


threadPostResponseToThreadPostRequest :: ThreadPostResponse -> ThreadPostRequest
threadPostResponseToThreadPostRequest  ThreadPostResponse{..} =
  ThreadPostRequest {
    threadPostRequestTitle = threadPostResponseTitle,
    threadPostRequestBody = threadPostResponseBody,
    threadPostRequestTags = threadPostResponseTags,
    threadPostRequestPrivateTags = threadPostResponsePrivateTags,
    threadPostRequestGuard = threadPostResponseGuard
  }


userRequestToUserResponse :: Int64 -> Text -> Text -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> UserRequest -> UserResponse
userRequestToUserResponse _1 _2 _3 _4 _5 _6 _7 _8 _9 UserRequest{..} =
  UserResponse {
    userResponseId = _1,
    userResponseName = _2,
    userResponseEmailMD5 = _3,
    userResponseActive = _4,
    userResponseGuard = _5,
    userResponseCreatedAt = _6,
    userResponseModifiedAt = _7,
    userResponseDeactivatedAt = _8,
    userResponseActivityAt = _9,
    userResponseDisplayName = userRequestDisplayName,
    userResponseFullName = userRequestFullName,
    userResponseEmail = userRequestEmail,
    userResponsePlugin = userRequestPlugin,
    userResponseIdent = userRequestIdent,
    userResponseAcceptTOS = userRequestAcceptTOS
  }


userResponseToUserRequest :: UserResponse -> UserRequest
userResponseToUserRequest  UserResponse{..} =
  UserRequest {
    userRequestDisplayName = userResponseDisplayName,
    userRequestFullName = userResponseFullName,
    userRequestEmail = userResponseEmail,
    userRequestPlugin = userResponsePlugin,
    userRequestIdent = userResponseIdent,
    userRequestAcceptTOS = userResponseAcceptTOS
  }


userRequestToUserSanitizedResponse :: Int64 -> Text -> Text -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> UserRequest -> UserSanitizedResponse
userRequestToUserSanitizedResponse _1 _2 _3 _4 _5 _6 _7 UserRequest{..} =
  UserSanitizedResponse {
    userSanitizedResponseId = _1,
    userSanitizedResponseName = _2,
    userSanitizedResponseEmailMD5 = _3,
    userSanitizedResponseActive = _4,
    userSanitizedResponseGuard = _5,
    userSanitizedResponseCreatedAt = _6,
    userSanitizedResponseActivityAt = _7,
    userSanitizedResponseDisplayName = userRequestDisplayName
  }


userSanitizedResponseToUserRequest :: Text -> Text -> Text -> Text -> (Maybe UTCTime) -> UserSanitizedResponse -> UserRequest
userSanitizedResponseToUserRequest _1 _2 _3 _4 _5 UserSanitizedResponse{..} =
  UserRequest {
    userRequestFullName = _1,
    userRequestEmail = _2,
    userRequestPlugin = _3,
    userRequestIdent = _4,
    userRequestAcceptTOS = _5,
    userRequestDisplayName = userSanitizedResponseDisplayName
  }

-- footer