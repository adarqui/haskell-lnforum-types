{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Convert where




import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Int            (Int64)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)

import LN.T.Internal.Types

apiRequestToApiResponse :: Int64 -> Int64 -> Text -> (Maybe Text) -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> ApiRequest -> ApiResponse
apiRequestToApiResponse apiResponseId apiResponseUserId apiResponseKey apiResponseComment apiResponseGuard apiResponseCreatedAt apiResponseModifiedAt ApiRequest{..} =
  ApiResponse {
    apiResponseId = apiResponseId,
    apiResponseUserId = apiResponseUserId,
    apiResponseKey = apiResponseKey,
    apiResponseComment = apiResponseComment,
    apiResponseGuard = apiResponseGuard,
    apiResponseCreatedAt = apiResponseCreatedAt,
    apiResponseModifiedAt = apiResponseModifiedAt
  }


apiResponseToApiRequest :: (Maybe Text) -> Int -> ApiResponse -> ApiRequest
apiResponseToApiRequest apiRequestComment apiRequestGuard ApiResponse{..} =
  ApiRequest {
    apiRequestComment = apiRequestComment,
    apiRequestGuard = apiRequestGuard
  }


boardRequestToBoardResponse :: Int64 -> Int64 -> Int64 -> Int64 -> (Maybe Int64) -> Text -> Text -> (Maybe Text) -> Bool -> Bool -> Bool -> [Text] -> (Maybe Text) -> [Text] -> Bool -> Int -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> BoardRequest -> BoardResponse
boardRequestToBoardResponse boardResponseId boardResponseUserId boardResponseOrgId boardResponseForumId boardResponseParentId boardResponseName boardResponseDisplayName boardResponseDescription boardResponseIsAnonymous boardResponseCanCreateSubBoards boardResponseCanCreateThreads boardResponseSuggestedTags boardResponseIcon boardResponseTags boardResponseActive boardResponseGuard boardResponseCreatedAt boardResponseModifiedBy boardResponseModifiedAt boardResponseActivityAt BoardRequest{..} =
  BoardResponse {
    boardResponseId = boardResponseId,
    boardResponseUserId = boardResponseUserId,
    boardResponseOrgId = boardResponseOrgId,
    boardResponseForumId = boardResponseForumId,
    boardResponseParentId = boardResponseParentId,
    boardResponseName = boardResponseName,
    boardResponseDisplayName = boardResponseDisplayName,
    boardResponseDescription = boardResponseDescription,
    boardResponseIsAnonymous = boardResponseIsAnonymous,
    boardResponseCanCreateSubBoards = boardResponseCanCreateSubBoards,
    boardResponseCanCreateThreads = boardResponseCanCreateThreads,
    boardResponseSuggestedTags = boardResponseSuggestedTags,
    boardResponseIcon = boardResponseIcon,
    boardResponseTags = boardResponseTags,
    boardResponseActive = boardResponseActive,
    boardResponseGuard = boardResponseGuard,
    boardResponseCreatedAt = boardResponseCreatedAt,
    boardResponseModifiedBy = boardResponseModifiedBy,
    boardResponseModifiedAt = boardResponseModifiedAt,
    boardResponseActivityAt = boardResponseActivityAt
  }


boardResponseToBoardRequest :: Text -> (Maybe Text) -> Bool -> Bool -> Bool -> [Text] -> (Maybe Text) -> [Text] -> Int -> BoardResponse -> BoardRequest
boardResponseToBoardRequest boardRequestDisplayName boardRequestDescription boardRequestIsAnonymous boardRequestCanCreateSubBoards boardRequestCanCreateThreads boardRequestSuggestedTags boardRequestIcon boardRequestTags boardRequestGuard BoardResponse{..} =
  BoardRequest {
    boardRequestDisplayName = boardRequestDisplayName,
    boardRequestDescription = boardRequestDescription,
    boardRequestIsAnonymous = boardRequestIsAnonymous,
    boardRequestCanCreateSubBoards = boardRequestCanCreateSubBoards,
    boardRequestCanCreateThreads = boardRequestCanCreateThreads,
    boardRequestSuggestedTags = boardRequestSuggestedTags,
    boardRequestIcon = boardRequestIcon,
    boardRequestTags = boardRequestTags,
    boardRequestGuard = boardRequestGuard
  }


bucketRequestToBucketResponse :: Int64 -> Int64 -> Text -> Text -> (Maybe Text) -> Int -> Int -> [Int64] -> [Int64] -> [Text] -> [Int64] -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> BucketRequest -> BucketResponse
bucketRequestToBucketResponse bucketResponseId bucketResponseUserId bucketResponseName bucketResponseDisplayName bucketResponseDescription bucketResponseScoreLo bucketResponseScoreHi bucketResponseLeurons bucketResponseResources bucketResponseCategories bucketResponseFilters bucketResponseActive bucketResponseGuard bucketResponseCreatedAt bucketResponseModifiedAt bucketResponseActivityAt BucketRequest{..} =
  BucketResponse {
    bucketResponseId = bucketResponseId,
    bucketResponseUserId = bucketResponseUserId,
    bucketResponseName = bucketResponseName,
    bucketResponseDisplayName = bucketResponseDisplayName,
    bucketResponseDescription = bucketResponseDescription,
    bucketResponseScoreLo = bucketResponseScoreLo,
    bucketResponseScoreHi = bucketResponseScoreHi,
    bucketResponseLeurons = bucketResponseLeurons,
    bucketResponseResources = bucketResponseResources,
    bucketResponseCategories = bucketResponseCategories,
    bucketResponseFilters = bucketResponseFilters,
    bucketResponseActive = bucketResponseActive,
    bucketResponseGuard = bucketResponseGuard,
    bucketResponseCreatedAt = bucketResponseCreatedAt,
    bucketResponseModifiedAt = bucketResponseModifiedAt,
    bucketResponseActivityAt = bucketResponseActivityAt
  }


bucketResponseToBucketRequest :: Text -> (Maybe Text) -> Int -> Int -> [Int64] -> [Int64] -> [Text] -> [Int64] -> Int -> BucketResponse -> BucketRequest
bucketResponseToBucketRequest bucketRequestDisplayName bucketRequestDescription bucketRequestScoreLo bucketRequestScoreHi bucketRequestLeurons bucketRequestResources bucketRequestCategories bucketRequestFilters bucketRequestGuard BucketResponse{..} =
  BucketRequest {
    bucketRequestDisplayName = bucketRequestDisplayName,
    bucketRequestDescription = bucketRequestDescription,
    bucketRequestScoreLo = bucketRequestScoreLo,
    bucketRequestScoreHi = bucketRequestScoreHi,
    bucketRequestLeurons = bucketRequestLeurons,
    bucketRequestResources = bucketRequestResources,
    bucketRequestCategories = bucketRequestCategories,
    bucketRequestFilters = bucketRequestFilters,
    bucketRequestGuard = bucketRequestGuard
  }


emptyRequestToEmptyResponse :: Int64 -> Int64 -> Bool -> (Maybe UTCTime) -> (Maybe UTCTime) -> EmptyRequest -> EmptyResponse
emptyRequestToEmptyResponse emptyResponseId emptyResponseUserId emptyResponseValue emptyResponseCreatedAt emptyResponseModifiedAt EmptyRequest{..} =
  EmptyResponse {
    emptyResponseId = emptyResponseId,
    emptyResponseUserId = emptyResponseUserId,
    emptyResponseValue = emptyResponseValue,
    emptyResponseCreatedAt = emptyResponseCreatedAt,
    emptyResponseModifiedAt = emptyResponseModifiedAt
  }


emptyResponseToEmptyRequest :: Bool -> EmptyResponse -> EmptyRequest
emptyResponseToEmptyRequest emptyRequestValue EmptyResponse{..} =
  EmptyRequest {
    emptyRequestValue = emptyRequestValue
  }


forumRequestToForumResponse :: Int64 -> Int64 -> Int64 -> Text -> Text -> (Maybe Text) -> Int -> Int -> Int -> Int -> Int -> (Maybe Text) -> [Text] -> Visibility -> Bool -> Int -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ForumRequest -> ForumResponse
forumRequestToForumResponse forumResponseId forumResponseUserId forumResponseOrgId forumResponseName forumResponseDisplayName forumResponseDescription forumResponseThreadsPerBoard forumResponseThreadPostsPerThread forumResponseRecentThreadsLimit forumResponseRecentPostsLimit forumResponseMotwLimit forumResponseIcon forumResponseTags forumResponseVisibility forumResponseActive forumResponseGuard forumResponseCreatedAt forumResponseModifiedBy forumResponseModifiedAt forumResponseActivityAt ForumRequest{..} =
  ForumResponse {
    forumResponseId = forumResponseId,
    forumResponseUserId = forumResponseUserId,
    forumResponseOrgId = forumResponseOrgId,
    forumResponseName = forumResponseName,
    forumResponseDisplayName = forumResponseDisplayName,
    forumResponseDescription = forumResponseDescription,
    forumResponseThreadsPerBoard = forumResponseThreadsPerBoard,
    forumResponseThreadPostsPerThread = forumResponseThreadPostsPerThread,
    forumResponseRecentThreadsLimit = forumResponseRecentThreadsLimit,
    forumResponseRecentPostsLimit = forumResponseRecentPostsLimit,
    forumResponseMotwLimit = forumResponseMotwLimit,
    forumResponseIcon = forumResponseIcon,
    forumResponseTags = forumResponseTags,
    forumResponseVisibility = forumResponseVisibility,
    forumResponseActive = forumResponseActive,
    forumResponseGuard = forumResponseGuard,
    forumResponseCreatedAt = forumResponseCreatedAt,
    forumResponseModifiedBy = forumResponseModifiedBy,
    forumResponseModifiedAt = forumResponseModifiedAt,
    forumResponseActivityAt = forumResponseActivityAt
  }


forumResponseToForumRequest :: Text -> (Maybe Text) -> Int -> Int -> Int -> Int -> Int -> (Maybe Text) -> [Text] -> Visibility -> Int -> ForumResponse -> ForumRequest
forumResponseToForumRequest forumRequestDisplayName forumRequestDescription forumRequestThreadsPerBoard forumRequestThreadPostsPerThread forumRequestRecentThreadsLimit forumRequestRecentPostsLimit forumRequestMotwLimit forumRequestIcon forumRequestTags forumRequestVisibility forumRequestGuard ForumResponse{..} =
  ForumRequest {
    forumRequestDisplayName = forumRequestDisplayName,
    forumRequestDescription = forumRequestDescription,
    forumRequestThreadsPerBoard = forumRequestThreadsPerBoard,
    forumRequestThreadPostsPerThread = forumRequestThreadPostsPerThread,
    forumRequestRecentThreadsLimit = forumRequestRecentThreadsLimit,
    forumRequestRecentPostsLimit = forumRequestRecentPostsLimit,
    forumRequestMotwLimit = forumRequestMotwLimit,
    forumRequestIcon = forumRequestIcon,
    forumRequestTags = forumRequestTags,
    forumRequestVisibility = forumRequestVisibility,
    forumRequestGuard = forumRequestGuard
  }


leuronRequestToLeuronResponse :: Int64 -> Int64 -> Int64 -> LeuronData -> (Maybe Text) -> (Maybe Text) -> (Maybe Text) -> (Maybe Text) -> (Maybe [Text]) -> (Maybe [Text]) -> (DepList Text) -> (Maybe [Splits]) -> (Maybe [Substitutions]) -> [Text] -> (Maybe [Text]) -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> LeuronRequest -> LeuronResponse
leuronRequestToLeuronResponse leuronResponseId leuronResponseUserId leuronResponseResourceId leuronResponseData leuronResponseTitle leuronResponseDescription leuronResponseSection leuronResponsePage leuronResponseExamples leuronResponseStrengths leuronResponseCategories leuronResponseSplits leuronResponseSubstitutions leuronResponseTags leuronResponseStyle leuronResponseActive leuronResponseGuard leuronResponseCreatedAt leuronResponseModifiedAt leuronResponseActivityAt LeuronRequest{..} =
  LeuronResponse {
    leuronResponseId = leuronResponseId,
    leuronResponseUserId = leuronResponseUserId,
    leuronResponseResourceId = leuronResponseResourceId,
    leuronResponseData = leuronResponseData,
    leuronResponseTitle = leuronResponseTitle,
    leuronResponseDescription = leuronResponseDescription,
    leuronResponseSection = leuronResponseSection,
    leuronResponsePage = leuronResponsePage,
    leuronResponseExamples = leuronResponseExamples,
    leuronResponseStrengths = leuronResponseStrengths,
    leuronResponseCategories = leuronResponseCategories,
    leuronResponseSplits = leuronResponseSplits,
    leuronResponseSubstitutions = leuronResponseSubstitutions,
    leuronResponseTags = leuronResponseTags,
    leuronResponseStyle = leuronResponseStyle,
    leuronResponseActive = leuronResponseActive,
    leuronResponseGuard = leuronResponseGuard,
    leuronResponseCreatedAt = leuronResponseCreatedAt,
    leuronResponseModifiedAt = leuronResponseModifiedAt,
    leuronResponseActivityAt = leuronResponseActivityAt
  }


leuronResponseToLeuronRequest :: LeuronData -> (Maybe Text) -> (Maybe Text) -> (Maybe Text) -> (Maybe Text) -> (Maybe [Text]) -> (Maybe [Text]) -> (DepList Text) -> (Maybe [Splits]) -> (Maybe [Substitutions]) -> [Text] -> (Maybe [Text]) -> Int -> LeuronResponse -> LeuronRequest
leuronResponseToLeuronRequest leuronRequestData leuronRequestTitle leuronRequestDescription leuronRequestSection leuronRequestPage leuronRequestExamples leuronRequestStrengths leuronRequestCategories leuronRequestSplits leuronRequestSubstitutions leuronRequestTags leuronRequestStyle leuronRequestGuard LeuronResponse{..} =
  LeuronRequest {
    leuronRequestData = leuronRequestData,
    leuronRequestTitle = leuronRequestTitle,
    leuronRequestDescription = leuronRequestDescription,
    leuronRequestSection = leuronRequestSection,
    leuronRequestPage = leuronRequestPage,
    leuronRequestExamples = leuronRequestExamples,
    leuronRequestStrengths = leuronRequestStrengths,
    leuronRequestCategories = leuronRequestCategories,
    leuronRequestSplits = leuronRequestSplits,
    leuronRequestSubstitutions = leuronRequestSubstitutions,
    leuronRequestTags = leuronRequestTags,
    leuronRequestStyle = leuronRequestStyle,
    leuronRequestGuard = leuronRequestGuard
  }


leuronTrainingRequestToLeuronTrainingResponse :: Int64 -> Int64 -> Int64 -> LeuronTrainingSummary -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> LeuronTrainingRequest -> LeuronTrainingResponse
leuronTrainingRequestToLeuronTrainingResponse leuronTrainingResponseId leuronTrainingResponseUserId leuronTrainingResponseLeuronId leuronTrainingResponseSummary leuronTrainingResponseGuard leuronTrainingResponseCreatedAt leuronTrainingResponseModifiedAt LeuronTrainingRequest{..} =
  LeuronTrainingResponse {
    leuronTrainingResponseId = leuronTrainingResponseId,
    leuronTrainingResponseUserId = leuronTrainingResponseUserId,
    leuronTrainingResponseLeuronId = leuronTrainingResponseLeuronId,
    leuronTrainingResponseSummary = leuronTrainingResponseSummary,
    leuronTrainingResponseGuard = leuronTrainingResponseGuard,
    leuronTrainingResponseCreatedAt = leuronTrainingResponseCreatedAt,
    leuronTrainingResponseModifiedAt = leuronTrainingResponseModifiedAt
  }


leuronTrainingResponseToLeuronTrainingRequest :: LeuronTrainingSummary -> Int -> LeuronTrainingResponse -> LeuronTrainingRequest
leuronTrainingResponseToLeuronTrainingRequest leuronTrainingRequestSummary leuronTrainingRequestGuard LeuronTrainingResponse{..} =
  LeuronTrainingRequest {
    leuronTrainingRequestSummary = leuronTrainingRequestSummary,
    leuronTrainingRequestGuard = leuronTrainingRequestGuard
  }


likeRequestToLikeResponse :: Int64 -> Ent -> Int64 -> Int64 -> LikeOpt -> Int -> (Maybe Text) -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> LikeRequest -> LikeResponse
likeRequestToLikeResponse likeResponseId likeResponseEnt likeResponseEntId likeResponseUserId likeResponseOpt likeResponseScore likeResponseReason likeResponseActive likeResponseGuard likeResponseCreatedAt likeResponseModifiedAt LikeRequest{..} =
  LikeResponse {
    likeResponseId = likeResponseId,
    likeResponseEnt = likeResponseEnt,
    likeResponseEntId = likeResponseEntId,
    likeResponseUserId = likeResponseUserId,
    likeResponseOpt = likeResponseOpt,
    likeResponseScore = likeResponseScore,
    likeResponseReason = likeResponseReason,
    likeResponseActive = likeResponseActive,
    likeResponseGuard = likeResponseGuard,
    likeResponseCreatedAt = likeResponseCreatedAt,
    likeResponseModifiedAt = likeResponseModifiedAt
  }


likeResponseToLikeRequest :: LikeOpt -> (Maybe Text) -> Int -> LikeResponse -> LikeRequest
likeResponseToLikeRequest likeRequestOpt likeRequestReason likeRequestGuard LikeResponse{..} =
  LikeRequest {
    likeRequestOpt = likeRequestOpt,
    likeRequestReason = likeRequestReason,
    likeRequestGuard = likeRequestGuard
  }


organizationRequestToOrganizationResponse :: Int64 -> Int64 -> Text -> Text -> (Maybe Text) -> Text -> Text -> Text -> Text -> Membership -> (Maybe Text) -> [Text] -> Visibility -> Bool -> Int -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> OrganizationRequest -> OrganizationResponse
organizationRequestToOrganizationResponse organizationResponseId organizationResponseUserId organizationResponseName organizationResponseDisplayName organizationResponseDescription organizationResponseCompany organizationResponseLocation organizationResponseEmail organizationResponseEmailMD5 organizationResponseMembership organizationResponseIcon organizationResponseTags organizationResponseVisibility organizationResponseActive organizationResponseGuard organizationResponseCreatedAt organizationResponseModifiedBy organizationResponseModifiedAt organizationResponseActivityAt OrganizationRequest{..} =
  OrganizationResponse {
    organizationResponseId = organizationResponseId,
    organizationResponseUserId = organizationResponseUserId,
    organizationResponseName = organizationResponseName,
    organizationResponseDisplayName = organizationResponseDisplayName,
    organizationResponseDescription = organizationResponseDescription,
    organizationResponseCompany = organizationResponseCompany,
    organizationResponseLocation = organizationResponseLocation,
    organizationResponseEmail = organizationResponseEmail,
    organizationResponseEmailMD5 = organizationResponseEmailMD5,
    organizationResponseMembership = organizationResponseMembership,
    organizationResponseIcon = organizationResponseIcon,
    organizationResponseTags = organizationResponseTags,
    organizationResponseVisibility = organizationResponseVisibility,
    organizationResponseActive = organizationResponseActive,
    organizationResponseGuard = organizationResponseGuard,
    organizationResponseCreatedAt = organizationResponseCreatedAt,
    organizationResponseModifiedBy = organizationResponseModifiedBy,
    organizationResponseModifiedAt = organizationResponseModifiedAt,
    organizationResponseActivityAt = organizationResponseActivityAt
  }


organizationResponseToOrganizationRequest :: Text -> (Maybe Text) -> Text -> Text -> Text -> Membership -> [Text] -> (Maybe Text) -> Visibility -> Int -> OrganizationResponse -> OrganizationRequest
organizationResponseToOrganizationRequest organizationRequestDisplayName organizationRequestDescription organizationRequestCompany organizationRequestLocation organizationRequestEmail organizationRequestMembership organizationRequestTags organizationRequestIcon organizationRequestVisibility organizationRequestGuard OrganizationResponse{..} =
  OrganizationRequest {
    organizationRequestDisplayName = organizationRequestDisplayName,
    organizationRequestDescription = organizationRequestDescription,
    organizationRequestCompany = organizationRequestCompany,
    organizationRequestLocation = organizationRequestLocation,
    organizationRequestEmail = organizationRequestEmail,
    organizationRequestMembership = organizationRequestMembership,
    organizationRequestTags = organizationRequestTags,
    organizationRequestIcon = organizationRequestIcon,
    organizationRequestVisibility = organizationRequestVisibility,
    organizationRequestGuard = organizationRequestGuard
  }


pmRequestToPmResponse :: Int64 -> Int64 -> Int64 -> Text -> Text -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> PmRequest -> PmResponse
pmRequestToPmResponse pmResponseId pmResponseUserId pmResponseToUserId pmResponseSubject pmResponseBody pmResponseActive pmResponseGuard pmResponseCreatedAt pmResponseModifiedAt pmResponseActivityAt PmRequest{..} =
  PmResponse {
    pmResponseId = pmResponseId,
    pmResponseUserId = pmResponseUserId,
    pmResponseToUserId = pmResponseToUserId,
    pmResponseSubject = pmResponseSubject,
    pmResponseBody = pmResponseBody,
    pmResponseActive = pmResponseActive,
    pmResponseGuard = pmResponseGuard,
    pmResponseCreatedAt = pmResponseCreatedAt,
    pmResponseModifiedAt = pmResponseModifiedAt,
    pmResponseActivityAt = pmResponseActivityAt
  }


pmResponseToPmRequest :: Text -> Text -> Int -> PmResponse -> PmRequest
pmResponseToPmRequest pmRequestSubject pmRequestBody pmRequestGuard PmResponse{..} =
  PmRequest {
    pmRequestSubject = pmRequestSubject,
    pmRequestBody = pmRequestBody,
    pmRequestGuard = pmRequestGuard
  }


pmInRequestToPmInResponse :: Int64 -> Int64 -> Int64 -> (Maybe Text) -> Bool -> Bool -> Bool -> Bool -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> PmInRequest -> PmInResponse
pmInRequestToPmInResponse pmInResponseId pmInResponsePmId pmInResponseUserId pmInResponseLabel pmInResponseIsRead pmInResponseIsStarred pmInResponseIsNew pmInResponseIsSaved pmInResponseActive pmInResponseGuard pmInResponseCreatedAt pmInResponseModifiedAt PmInRequest{..} =
  PmInResponse {
    pmInResponseId = pmInResponseId,
    pmInResponsePmId = pmInResponsePmId,
    pmInResponseUserId = pmInResponseUserId,
    pmInResponseLabel = pmInResponseLabel,
    pmInResponseIsRead = pmInResponseIsRead,
    pmInResponseIsStarred = pmInResponseIsStarred,
    pmInResponseIsNew = pmInResponseIsNew,
    pmInResponseIsSaved = pmInResponseIsSaved,
    pmInResponseActive = pmInResponseActive,
    pmInResponseGuard = pmInResponseGuard,
    pmInResponseCreatedAt = pmInResponseCreatedAt,
    pmInResponseModifiedAt = pmInResponseModifiedAt
  }


pmInResponseToPmInRequest :: (Maybe Text) -> Bool -> Bool -> Int -> PmInResponse -> PmInRequest
pmInResponseToPmInRequest pmInRequestLabel pmInRequestIsRead pmInRequestIsStarred pmInRequestGuard PmInResponse{..} =
  PmInRequest {
    pmInRequestLabel = pmInRequestLabel,
    pmInRequestIsRead = pmInRequestIsRead,
    pmInRequestIsStarred = pmInRequestIsStarred,
    pmInRequestGuard = pmInRequestGuard
  }


pmOutRequestToPmOutResponse :: Int64 -> Int64 -> Int64 -> (Maybe Text) -> Bool -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> PmOutRequest -> PmOutResponse
pmOutRequestToPmOutResponse pmOutResponseId pmOutResponsePmId pmOutResponseUserId pmOutResponseLabel pmOutResponseIsSaved pmOutResponseActive pmOutResponseGuard pmOutResponseCreatedAt pmOutResponseModifiedAt PmOutRequest{..} =
  PmOutResponse {
    pmOutResponseId = pmOutResponseId,
    pmOutResponsePmId = pmOutResponsePmId,
    pmOutResponseUserId = pmOutResponseUserId,
    pmOutResponseLabel = pmOutResponseLabel,
    pmOutResponseIsSaved = pmOutResponseIsSaved,
    pmOutResponseActive = pmOutResponseActive,
    pmOutResponseGuard = pmOutResponseGuard,
    pmOutResponseCreatedAt = pmOutResponseCreatedAt,
    pmOutResponseModifiedAt = pmOutResponseModifiedAt
  }


pmOutResponseToPmOutRequest :: (Maybe Text) -> Int -> PmOutResponse -> PmOutRequest
pmOutResponseToPmOutRequest pmOutRequestLabel pmOutRequestGuard PmOutResponse{..} =
  PmOutRequest {
    pmOutRequestLabel = pmOutRequestLabel,
    pmOutRequestGuard = pmOutRequestGuard
  }


profileRequestToProfileResponse :: Int64 -> Ent -> Int64 -> ProfileGender -> UTCTime -> (Maybe Text) -> (Maybe Text) -> (Maybe Text) -> Bool -> Int -> Int -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> ProfileRequest -> ProfileResponse
profileRequestToProfileResponse profileResponseId profileResponseEnt profileResponseEntId profileResponseGender profileResponseBirthdate profileResponseWebsite profileResponseLocation profileResponseSignature profileResponseDebug profileResponseKarmaGood profileResponseKarmaBad profileResponseGuard profileResponseCreatedAt profileResponseModifiedAt ProfileRequest{..} =
  ProfileResponse {
    profileResponseId = profileResponseId,
    profileResponseEnt = profileResponseEnt,
    profileResponseEntId = profileResponseEntId,
    profileResponseGender = profileResponseGender,
    profileResponseBirthdate = profileResponseBirthdate,
    profileResponseWebsite = profileResponseWebsite,
    profileResponseLocation = profileResponseLocation,
    profileResponseSignature = profileResponseSignature,
    profileResponseDebug = profileResponseDebug,
    profileResponseKarmaGood = profileResponseKarmaGood,
    profileResponseKarmaBad = profileResponseKarmaBad,
    profileResponseGuard = profileResponseGuard,
    profileResponseCreatedAt = profileResponseCreatedAt,
    profileResponseModifiedAt = profileResponseModifiedAt
  }


profileResponseToProfileRequest :: ProfileGender -> UTCTime -> (Maybe Text) -> (Maybe Text) -> (Maybe Text) -> Bool -> Int -> ProfileResponse -> ProfileRequest
profileResponseToProfileRequest profileRequestGender profileRequestBirthdate profileRequestWebsite profileRequestLocation profileRequestSignature profileRequestDebug profileRequestGuard ProfileResponse{..} =
  ProfileRequest {
    profileRequestGender = profileRequestGender,
    profileRequestBirthdate = profileRequestBirthdate,
    profileRequestWebsite = profileRequestWebsite,
    profileRequestLocation = profileRequestLocation,
    profileRequestSignature = profileRequestSignature,
    profileRequestDebug = profileRequestDebug,
    profileRequestGuard = profileRequestGuard
  }


reminderRequestToReminderResponse :: Int64 -> Int64 -> Int64 -> Text -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ReminderRequest -> ReminderResponse
reminderRequestToReminderResponse reminderResponseId reminderResponseUserId reminderResponseParentFolderId reminderResponseData reminderResponseActive reminderResponseGuard reminderResponseCreatedAt reminderResponseModifiedAt reminderResponseActivityAt ReminderRequest{..} =
  ReminderResponse {
    reminderResponseId = reminderResponseId,
    reminderResponseUserId = reminderResponseUserId,
    reminderResponseParentFolderId = reminderResponseParentFolderId,
    reminderResponseData = reminderResponseData,
    reminderResponseActive = reminderResponseActive,
    reminderResponseGuard = reminderResponseGuard,
    reminderResponseCreatedAt = reminderResponseCreatedAt,
    reminderResponseModifiedAt = reminderResponseModifiedAt,
    reminderResponseActivityAt = reminderResponseActivityAt
  }


reminderResponseToReminderRequest :: Text -> Int -> ReminderResponse -> ReminderRequest
reminderResponseToReminderRequest reminderRequestData reminderRequestGuard ReminderResponse{..} =
  ReminderRequest {
    reminderRequestData = reminderRequestData,
    reminderRequestGuard = reminderRequestGuard
  }


reminderFolderRequestToReminderFolderResponse :: Int64 -> Int64 -> (Maybe Int64) -> Text -> Text -> Visibility -> (Maybe Text) -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ReminderFolderRequest -> ReminderFolderResponse
reminderFolderRequestToReminderFolderResponse reminderFolderResponseId reminderFolderResponseUserId reminderFolderResponseParentFolderId reminderFolderResponseName reminderFolderResponseDisplayName reminderFolderResponseVisibility reminderFolderResponseDescription reminderFolderResponseActive reminderFolderResponseGuard reminderFolderResponseCreatedAt reminderFolderResponseModifiedAt reminderFolderResponseActivityAt ReminderFolderRequest{..} =
  ReminderFolderResponse {
    reminderFolderResponseId = reminderFolderResponseId,
    reminderFolderResponseUserId = reminderFolderResponseUserId,
    reminderFolderResponseParentFolderId = reminderFolderResponseParentFolderId,
    reminderFolderResponseName = reminderFolderResponseName,
    reminderFolderResponseDisplayName = reminderFolderResponseDisplayName,
    reminderFolderResponseVisibility = reminderFolderResponseVisibility,
    reminderFolderResponseDescription = reminderFolderResponseDescription,
    reminderFolderResponseActive = reminderFolderResponseActive,
    reminderFolderResponseGuard = reminderFolderResponseGuard,
    reminderFolderResponseCreatedAt = reminderFolderResponseCreatedAt,
    reminderFolderResponseModifiedAt = reminderFolderResponseModifiedAt,
    reminderFolderResponseActivityAt = reminderFolderResponseActivityAt
  }


reminderFolderResponseToReminderFolderRequest :: Text -> (Maybe Text) -> Visibility -> Int -> ReminderFolderResponse -> ReminderFolderRequest
reminderFolderResponseToReminderFolderRequest reminderFolderRequestDisplayName reminderFolderRequestDescription reminderFolderRequestVisibility reminderFolderRequestGuard ReminderFolderResponse{..} =
  ReminderFolderRequest {
    reminderFolderRequestDisplayName = reminderFolderRequestDisplayName,
    reminderFolderRequestDescription = reminderFolderRequestDescription,
    reminderFolderRequestVisibility = reminderFolderRequestVisibility,
    reminderFolderRequestGuard = reminderFolderRequestGuard
  }


resourceRequestToResourceResponse :: Int64 -> Int64 -> Text -> Text -> Text -> ResourceType -> (Maybe [Text]) -> (DepList Text) -> (DepList Text) -> Visibility -> Int -> (Maybe Text) -> (Maybe [Text]) -> (Maybe Text) -> [Text] -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ResourceRequest -> ResourceResponse
resourceRequestToResourceResponse resourceResponseId resourceResponseUserId resourceResponseName resourceResponseDisplayName resourceResponseDescription resourceResponseSource resourceResponseAuthor resourceResponsePrerequisites resourceResponseCategories resourceResponseVisibility resourceResponseCounter resourceResponseVersion resourceResponseUrls resourceResponseIcon resourceResponseTags resourceResponseActive resourceResponseGuard resourceResponseCreatedAt resourceResponseModifiedAt resourceResponseActivityAt ResourceRequest{..} =
  ResourceResponse {
    resourceResponseId = resourceResponseId,
    resourceResponseUserId = resourceResponseUserId,
    resourceResponseName = resourceResponseName,
    resourceResponseDisplayName = resourceResponseDisplayName,
    resourceResponseDescription = resourceResponseDescription,
    resourceResponseSource = resourceResponseSource,
    resourceResponseAuthor = resourceResponseAuthor,
    resourceResponsePrerequisites = resourceResponsePrerequisites,
    resourceResponseCategories = resourceResponseCategories,
    resourceResponseVisibility = resourceResponseVisibility,
    resourceResponseCounter = resourceResponseCounter,
    resourceResponseVersion = resourceResponseVersion,
    resourceResponseUrls = resourceResponseUrls,
    resourceResponseIcon = resourceResponseIcon,
    resourceResponseTags = resourceResponseTags,
    resourceResponseActive = resourceResponseActive,
    resourceResponseGuard = resourceResponseGuard,
    resourceResponseCreatedAt = resourceResponseCreatedAt,
    resourceResponseModifiedAt = resourceResponseModifiedAt,
    resourceResponseActivityAt = resourceResponseActivityAt
  }


resourceResponseToResourceRequest :: Text -> Text -> ResourceType -> (Maybe [Text]) -> (DepList Text) -> (DepList Text) -> Visibility -> Int -> (Maybe Text) -> (Maybe [Text]) -> (Maybe Text) -> [Text] -> Int -> ResourceResponse -> ResourceRequest
resourceResponseToResourceRequest resourceRequestDisplayName resourceRequestDescription resourceRequestSource resourceRequestAuthor resourceRequestPrerequisites resourceRequestCategories resourceRequestVisibility resourceRequestCounter resourceRequestVersion resourceRequestUrls resourceRequestIcon resourceRequestTags resourceRequestGuard ResourceResponse{..} =
  ResourceRequest {
    resourceRequestDisplayName = resourceRequestDisplayName,
    resourceRequestDescription = resourceRequestDescription,
    resourceRequestSource = resourceRequestSource,
    resourceRequestAuthor = resourceRequestAuthor,
    resourceRequestPrerequisites = resourceRequestPrerequisites,
    resourceRequestCategories = resourceRequestCategories,
    resourceRequestVisibility = resourceRequestVisibility,
    resourceRequestCounter = resourceRequestCounter,
    resourceRequestVersion = resourceRequestVersion,
    resourceRequestUrls = resourceRequestUrls,
    resourceRequestIcon = resourceRequestIcon,
    resourceRequestTags = resourceRequestTags,
    resourceRequestGuard = resourceRequestGuard
  }


starRequestToStarResponse :: Int64 -> Ent -> Int64 -> Int64 -> (Maybe Text) -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> StarRequest -> StarResponse
starRequestToStarResponse starResponseId starResponseEnt starResponseEntId starResponseUserId starResponseReason starResponseActive starResponseGuard starResponseCreatedAt starResponseModifiedAt StarRequest{..} =
  StarResponse {
    starResponseId = starResponseId,
    starResponseEnt = starResponseEnt,
    starResponseEntId = starResponseEntId,
    starResponseUserId = starResponseUserId,
    starResponseReason = starResponseReason,
    starResponseActive = starResponseActive,
    starResponseGuard = starResponseGuard,
    starResponseCreatedAt = starResponseCreatedAt,
    starResponseModifiedAt = starResponseModifiedAt
  }


starResponseToStarRequest :: (Maybe Text) -> Int -> StarResponse -> StarRequest
starResponseToStarRequest starRequestReason starRequestGuard StarResponse{..} =
  StarRequest {
    starRequestReason = starRequestReason,
    starRequestGuard = starRequestGuard
  }


teamRequestToTeamResponse :: Int64 -> Int64 -> Int64 -> SystemTeam -> Membership -> (Maybe Text) -> [Text] -> Visibility -> Bool -> Int -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> TeamRequest -> TeamResponse
teamRequestToTeamResponse teamResponseId teamResponseUserId teamResponseOrgId teamResponseSystem teamResponseMembership teamResponseIcon teamResponseTags teamResponseVisibility teamResponseActive teamResponseGuard teamResponseCreatedAt teamResponseModifiedBy teamResponseModifiedAt teamResponseActivityAt TeamRequest{..} =
  TeamResponse {
    teamResponseId = teamResponseId,
    teamResponseUserId = teamResponseUserId,
    teamResponseOrgId = teamResponseOrgId,
    teamResponseSystem = teamResponseSystem,
    teamResponseMembership = teamResponseMembership,
    teamResponseIcon = teamResponseIcon,
    teamResponseTags = teamResponseTags,
    teamResponseVisibility = teamResponseVisibility,
    teamResponseActive = teamResponseActive,
    teamResponseGuard = teamResponseGuard,
    teamResponseCreatedAt = teamResponseCreatedAt,
    teamResponseModifiedBy = teamResponseModifiedBy,
    teamResponseModifiedAt = teamResponseModifiedAt,
    teamResponseActivityAt = teamResponseActivityAt
  }


teamResponseToTeamRequest :: Membership -> (Maybe Text) -> [Text] -> Visibility -> Int -> TeamResponse -> TeamRequest
teamResponseToTeamRequest teamRequestMembership teamRequestIcon teamRequestTags teamRequestVisibility teamRequestGuard TeamResponse{..} =
  TeamRequest {
    teamRequestMembership = teamRequestMembership,
    teamRequestIcon = teamRequestIcon,
    teamRequestTags = teamRequestTags,
    teamRequestVisibility = teamRequestVisibility,
    teamRequestGuard = teamRequestGuard
  }


threadRequestToThreadResponse :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Text -> Text -> (Maybe Text) -> Bool -> Bool -> (Maybe Text) -> (Maybe Text) -> [Text] -> Bool -> Int -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ThreadRequest -> ThreadResponse
threadRequestToThreadResponse threadResponseId threadResponseUserId threadResponseOrgId threadResponseForumId threadResponseBoardId threadResponseName threadResponseDisplayName threadResponseDescription threadResponseSticky threadResponseLocked threadResponsePoll threadResponseIcon threadResponseTags threadResponseActive threadResponseGuard threadResponseCreatedAt threadResponseModifiedBy threadResponseModifiedAt threadResponseActivityAt ThreadRequest{..} =
  ThreadResponse {
    threadResponseId = threadResponseId,
    threadResponseUserId = threadResponseUserId,
    threadResponseOrgId = threadResponseOrgId,
    threadResponseForumId = threadResponseForumId,
    threadResponseBoardId = threadResponseBoardId,
    threadResponseName = threadResponseName,
    threadResponseDisplayName = threadResponseDisplayName,
    threadResponseDescription = threadResponseDescription,
    threadResponseSticky = threadResponseSticky,
    threadResponseLocked = threadResponseLocked,
    threadResponsePoll = threadResponsePoll,
    threadResponseIcon = threadResponseIcon,
    threadResponseTags = threadResponseTags,
    threadResponseActive = threadResponseActive,
    threadResponseGuard = threadResponseGuard,
    threadResponseCreatedAt = threadResponseCreatedAt,
    threadResponseModifiedBy = threadResponseModifiedBy,
    threadResponseModifiedAt = threadResponseModifiedAt,
    threadResponseActivityAt = threadResponseActivityAt
  }


threadResponseToThreadRequest :: Text -> (Maybe Text) -> Bool -> Bool -> (Maybe Text) -> (Maybe Text) -> [Text] -> Int -> ThreadResponse -> ThreadRequest
threadResponseToThreadRequest threadRequestDisplayName threadRequestDescription threadRequestSticky threadRequestLocked threadRequestPoll threadRequestIcon threadRequestTags threadRequestGuard ThreadResponse{..} =
  ThreadRequest {
    threadRequestDisplayName = threadRequestDisplayName,
    threadRequestDescription = threadRequestDescription,
    threadRequestSticky = threadRequestSticky,
    threadRequestLocked = threadRequestLocked,
    threadRequestPoll = threadRequestPoll,
    threadRequestIcon = threadRequestIcon,
    threadRequestTags = threadRequestTags,
    threadRequestGuard = threadRequestGuard
  }


threadPostRequestToThreadPostResponse :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> (Maybe Int64) -> (Maybe Text) -> PostData -> [Text] -> [Text] -> Bool -> Int -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ThreadPostRequest -> ThreadPostResponse
threadPostRequestToThreadPostResponse threadPostResponseId threadPostResponseUserId threadPostResponseOrgId threadPostResponseForumId threadPostResponseBoardId threadPostResponseThreadId threadPostResponseParentId threadPostResponseTitle threadPostResponseBody threadPostResponseTags threadPostResponsePrivateTags threadPostResponseActive threadPostResponseGuard threadPostResponseCreatedAt threadPostResponseModifiedBy threadPostResponseModifiedAt threadPostResponseActivityAt ThreadPostRequest{..} =
  ThreadPostResponse {
    threadPostResponseId = threadPostResponseId,
    threadPostResponseUserId = threadPostResponseUserId,
    threadPostResponseOrgId = threadPostResponseOrgId,
    threadPostResponseForumId = threadPostResponseForumId,
    threadPostResponseBoardId = threadPostResponseBoardId,
    threadPostResponseThreadId = threadPostResponseThreadId,
    threadPostResponseParentId = threadPostResponseParentId,
    threadPostResponseTitle = threadPostResponseTitle,
    threadPostResponseBody = threadPostResponseBody,
    threadPostResponseTags = threadPostResponseTags,
    threadPostResponsePrivateTags = threadPostResponsePrivateTags,
    threadPostResponseActive = threadPostResponseActive,
    threadPostResponseGuard = threadPostResponseGuard,
    threadPostResponseCreatedAt = threadPostResponseCreatedAt,
    threadPostResponseModifiedBy = threadPostResponseModifiedBy,
    threadPostResponseModifiedAt = threadPostResponseModifiedAt,
    threadPostResponseActivityAt = threadPostResponseActivityAt
  }


threadPostResponseToThreadPostRequest :: (Maybe Text) -> PostData -> [Text] -> [Text] -> Int -> ThreadPostResponse -> ThreadPostRequest
threadPostResponseToThreadPostRequest threadPostRequestTitle threadPostRequestBody threadPostRequestTags threadPostRequestPrivateTags threadPostRequestGuard ThreadPostResponse{..} =
  ThreadPostRequest {
    threadPostRequestTitle = threadPostRequestTitle,
    threadPostRequestBody = threadPostRequestBody,
    threadPostRequestTags = threadPostRequestTags,
    threadPostRequestPrivateTags = threadPostRequestPrivateTags,
    threadPostRequestGuard = threadPostRequestGuard
  }


userRequestToUserResponse :: Int64 -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> (Maybe UTCTime) -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> UserRequest -> UserResponse
userRequestToUserResponse userResponseId userResponseName userResponseDisplayName userResponseFullName userResponseEmail userResponseEmailMD5 userResponsePlugin userResponseIdent userResponseAcceptTOS userResponseActive userResponseGuard userResponseCreatedAt userResponseModifiedAt userResponseDeactivatedAt userResponseActivityAt UserRequest{..} =
  UserResponse {
    userResponseId = userResponseId,
    userResponseName = userResponseName,
    userResponseDisplayName = userResponseDisplayName,
    userResponseFullName = userResponseFullName,
    userResponseEmail = userResponseEmail,
    userResponseEmailMD5 = userResponseEmailMD5,
    userResponsePlugin = userResponsePlugin,
    userResponseIdent = userResponseIdent,
    userResponseAcceptTOS = userResponseAcceptTOS,
    userResponseActive = userResponseActive,
    userResponseGuard = userResponseGuard,
    userResponseCreatedAt = userResponseCreatedAt,
    userResponseModifiedAt = userResponseModifiedAt,
    userResponseDeactivatedAt = userResponseDeactivatedAt,
    userResponseActivityAt = userResponseActivityAt
  }


userResponseToUserRequest :: Text -> Text -> Text -> Text -> Text -> (Maybe UTCTime) -> UserResponse -> UserRequest
userResponseToUserRequest userRequestDisplayName userRequestFullName userRequestEmail userRequestPlugin userRequestIdent userRequestAcceptTOS UserResponse{..} =
  UserRequest {
    userRequestDisplayName = userRequestDisplayName,
    userRequestFullName = userRequestFullName,
    userRequestEmail = userRequestEmail,
    userRequestPlugin = userRequestPlugin,
    userRequestIdent = userRequestIdent,
    userRequestAcceptTOS = userRequestAcceptTOS
  }


userRequestToUserSanitizedResponse :: Int64 -> Text -> Text -> Text -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> UserRequest -> UserSanitizedResponse
userRequestToUserSanitizedResponse userSanitizedResponseId userSanitizedResponseName userSanitizedResponseDisplayName userSanitizedResponseEmailMD5 userSanitizedResponseActive userSanitizedResponseGuard userSanitizedResponseCreatedAt userSanitizedResponseActivityAt UserRequest{..} =
  UserSanitizedResponse {
    userSanitizedResponseId = userSanitizedResponseId,
    userSanitizedResponseName = userSanitizedResponseName,
    userSanitizedResponseDisplayName = userSanitizedResponseDisplayName,
    userSanitizedResponseEmailMD5 = userSanitizedResponseEmailMD5,
    userSanitizedResponseActive = userSanitizedResponseActive,
    userSanitizedResponseGuard = userSanitizedResponseGuard,
    userSanitizedResponseCreatedAt = userSanitizedResponseCreatedAt,
    userSanitizedResponseActivityAt = userSanitizedResponseActivityAt
  }


userSanitizedResponseToUserRequest :: Text -> Text -> Text -> Text -> Text -> (Maybe UTCTime) -> UserSanitizedResponse -> UserRequest
userSanitizedResponseToUserRequest userRequestDisplayName userRequestFullName userRequestEmail userRequestPlugin userRequestIdent userRequestAcceptTOS UserSanitizedResponse{..} =
  UserRequest {
    userRequestDisplayName = userRequestDisplayName,
    userRequestFullName = userRequestFullName,
    userRequestEmail = userRequestEmail,
    userRequestPlugin = userRequestPlugin,
    userRequestIdent = userRequestIdent,
    userRequestAcceptTOS = userRequestAcceptTOS
  }

-- footer