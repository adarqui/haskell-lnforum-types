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
import           Prelude

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


idRequestToIdResponse :: Int64 -> Int64 -> Int64 -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> IdRequest -> IdResponse
idRequestToIdResponse _1 _2 _3 _4 _5 _6 IdRequest{..} =
  IdResponse {
    idResponseId = _1,
    idResponseUserId = _2,
    idResponseGuard = _3,
    idResponseCreatedAt = _4,
    idResponseModifiedAt = _5,
    idResponseActivityAt = _6,
    idResponseTargetId = idRequestTargetId
  }


idResponseToIdRequest :: Int -> IdResponse -> IdRequest
idResponseToIdRequest _1 IdResponse{..} =
  IdRequest {
    idRequestGuard = _1,
    idRequestTargetId = idResponseTargetId
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


profileResponseToProfileRequest :: [Text] -> (Maybe Text) -> ProfileResponse -> ProfileRequest
profileResponseToProfileRequest _1 _2 ProfileResponse{..} =
  ProfileRequest {
    profileRequestWebsites = _1,
    profileRequestStateWebsites = _2,
    profileRequestGender = profileResponseGender,
    profileRequestBirthdate = profileResponseBirthdate,
    profileRequestWebsite = profileResponseWebsite,
    profileRequestLocation = profileResponseLocation,
    profileRequestSignature = profileResponseSignature,
    profileRequestDebug = profileResponseDebug,
    profileRequestGuard = profileResponseGuard
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


userRequestToUserResponse :: Int64 -> Text -> Text -> (Maybe Text) -> (Maybe UTCTime) -> (Maybe Text) -> (Maybe UTCTime) -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> UserRequest -> UserResponse
userRequestToUserResponse _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 UserRequest{..} =
  UserResponse {
    userResponseId = _1,
    userResponseName = _2,
    userResponseEmailMD5 = _3,
    userResponseGithubIdent = _4,
    userResponseGithubCreatedAt = _5,
    userResponseGoogleIdent = _6,
    userResponseGoogleCreatedAt = _7,
    userResponseActive = _8,
    userResponseGuard = _9,
    userResponseCreatedAt = _10,
    userResponseModifiedAt = _11,
    userResponseDeactivatedAt = _12,
    userResponseActivityAt = _13,
    userResponseDisplayName = userRequestDisplayName,
    userResponseFullName = userRequestFullName,
    userResponseEmail = userRequestEmail,
    userResponsePlugin = userRequestPlugin,
    userResponseAcceptTOS = userRequestAcceptTOS
  }


userResponseToUserRequest :: UserResponse -> UserRequest
userResponseToUserRequest  UserResponse{..} =
  UserRequest {
    userRequestDisplayName = userResponseDisplayName,
    userRequestFullName = userResponseFullName,
    userRequestEmail = userResponseEmail,
    userRequestPlugin = userResponsePlugin,
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


userSanitizedResponseToUserRequest :: Text -> Text -> Text -> (Maybe UTCTime) -> UserSanitizedResponse -> UserRequest
userSanitizedResponseToUserRequest _1 _2 _3 _4 UserSanitizedResponse{..} =
  UserRequest {
    userRequestFullName = _1,
    userRequestEmail = _2,
    userRequestPlugin = _3,
    userRequestAcceptTOS = _4,
    userRequestDisplayName = userSanitizedResponseDisplayName
  }


simpleStringRequestToSimpleStringResponse :: SimpleStringRequest -> SimpleStringResponse
simpleStringRequestToSimpleStringResponse  SimpleStringRequest{..} =
  SimpleStringResponse {
    simpleStringResponse = simpleStringRequest
  }


simpleStringsRequestToSimpleStringsResponse :: SimpleStringsRequest -> SimpleStringsResponse
simpleStringsRequestToSimpleStringsResponse  SimpleStringsRequest{..} =
  SimpleStringsResponse {
    simpleStringsResponse = simpleStringsRequest
  }


simpleStringRequestToSimpleStringResponse :: SimpleStringRequest -> SimpleStringResponse
simpleStringRequestToSimpleStringResponse  SimpleStringRequest{..} =
  SimpleStringResponse {
    simpleStringResponse = simpleStringRequest
  }


simpleStringsRequestToSimpleStringsResponse :: SimpleStringsRequest -> SimpleStringsResponse
simpleStringsRequestToSimpleStringsResponse  SimpleStringsRequest{..} =
  SimpleStringsResponse {
    simpleStringsResponse = simpleStringsRequest
  }

-- footer