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


boardRequestToBoardResponse :: Int64 -> Int64 -> Text -> Bool -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> BoardRequest -> BoardResponse
boardRequestToBoardResponse _1 _2 _3 _4 _5 _6 _7 BoardRequest{..} =
  BoardResponse {
    boardResponseId = _1,
    boardResponseUserId = _2,
    boardResponseName = _3,
    boardResponseActive = _4,
    boardResponseCreatedAt = _5,
    boardResponseModifiedAt = _6,
    boardResponseActivityAt = _7,
    boardResponseDisplayName = boardRequestDisplayName,
    boardResponseDescription = boardRequestDescription,
    boardResponseSource = boardRequestSource,
    boardResponseAuthor = boardRequestAuthor,
    boardResponsePrerequisites = boardRequestPrerequisites,
    boardResponseCategories = boardRequestCategories,
    boardResponseVisibility = boardRequestVisibility,
    boardResponseCounter = boardRequestCounter,
    boardResponseVersion = boardRequestVersion,
    boardResponseUrls = boardRequestUrls,
    boardResponseIcon = boardRequestIcon,
    boardResponseTags = boardRequestTags,
    boardResponseGuard = boardRequestGuard
  }


boardResponseToBoardRequest :: BoardResponse -> BoardRequest
boardResponseToBoardRequest  BoardResponse{..} =
  BoardRequest {
    boardRequestDisplayName = boardResponseDisplayName,
    boardRequestDescription = boardResponseDescription,
    boardRequestSource = boardResponseSource,
    boardRequestAuthor = boardResponseAuthor,
    boardRequestPrerequisites = boardResponsePrerequisites,
    boardRequestCategories = boardResponseCategories,
    boardRequestVisibility = boardResponseVisibility,
    boardRequestCounter = boardResponseCounter,
    boardRequestVersion = boardResponseVersion,
    boardRequestUrls = boardResponseUrls,
    boardRequestIcon = boardResponseIcon,
    boardRequestTags = boardResponseTags,
    boardRequestGuard = boardResponseGuard
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

-- footer