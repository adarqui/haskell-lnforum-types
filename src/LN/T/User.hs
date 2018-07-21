{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.User where





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

data UserRequest = UserRequest {
  userRequestDisplayName :: !(Text),
  userRequestFullName :: !(Text),
  userRequestEmail :: !(Text),
  userRequestPlugin :: !(Text),
  userRequestAcceptTOS :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON UserRequest where
  parseJSON (Object o) = do
    userRequestDisplayName <- o .: ("display_name" :: Text)
    userRequestFullName <- o .: ("full_name" :: Text)
    userRequestEmail <- o .: ("email" :: Text)
    userRequestPlugin <- o .: ("plugin" :: Text)
    userRequestAcceptTOS <- o .: ("accept_tos" :: Text)
    pure $ UserRequest {
      userRequestDisplayName = userRequestDisplayName,
      userRequestFullName = userRequestFullName,
      userRequestEmail = userRequestEmail,
      userRequestPlugin = userRequestPlugin,
      userRequestAcceptTOS = userRequestAcceptTOS
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON UserRequest where
  toJSON UserRequest{..} = object $
    [ "tag" .= ("UserRequest" :: Text)
    , "display_name" .= userRequestDisplayName
    , "full_name" .= userRequestFullName
    , "email" .= userRequestEmail
    , "plugin" .= userRequestPlugin
    , "accept_tos" .= userRequestAcceptTOS
    ]


instance Eq UserRequest where
  (==) a b = userRequestDisplayName a == userRequestDisplayName b && userRequestFullName a == userRequestFullName b && userRequestEmail a == userRequestEmail b && userRequestPlugin a == userRequestPlugin b && userRequestAcceptTOS a == userRequestAcceptTOS b

instance Show UserRequest where
    show rec = "userRequestDisplayName: " <> show (userRequestDisplayName rec) <> ", " <> "userRequestFullName: " <> show (userRequestFullName rec) <> ", " <> "userRequestEmail: " <> show (userRequestEmail rec) <> ", " <> "userRequestPlugin: " <> show (userRequestPlugin rec) <> ", " <> "userRequestAcceptTOS: " <> show (userRequestAcceptTOS rec)

data UserResponse = UserResponse {
  userResponseId :: !(Int64),
  userResponseName :: !(Text),
  userResponseDisplayName :: !(Text),
  userResponseFullName :: !(Text),
  userResponseEmail :: !(Text),
  userResponseEmailMD5 :: !(Text),
  userResponsePlugin :: !(Text),
  userResponseGithubIdent :: !((Maybe Text)),
  userResponseGithubCreatedAt :: !((Maybe UTCTime)),
  userResponseGoogleIdent :: !((Maybe Text)),
  userResponseGoogleCreatedAt :: !((Maybe UTCTime)),
  userResponseAcceptTOS :: !((Maybe UTCTime)),
  userResponseActive :: !(Bool),
  userResponseGuard :: !(Int),
  userResponseCreatedAt :: !((Maybe UTCTime)),
  userResponseModifiedAt :: !((Maybe UTCTime)),
  userResponseDeactivatedAt :: !((Maybe UTCTime)),
  userResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON UserResponse where
  parseJSON (Object o) = do
    userResponseId <- o .: ("id" :: Text)
    userResponseName <- o .: ("name" :: Text)
    userResponseDisplayName <- o .: ("display_name" :: Text)
    userResponseFullName <- o .: ("full_name" :: Text)
    userResponseEmail <- o .: ("email" :: Text)
    userResponseEmailMD5 <- o .: ("email_md5" :: Text)
    userResponsePlugin <- o .: ("plugin" :: Text)
    userResponseGithubIdent <- o .: ("github_ident" :: Text)
    userResponseGithubCreatedAt <- o .: ("github_created_at" :: Text)
    userResponseGoogleIdent <- o .: ("google_ident" :: Text)
    userResponseGoogleCreatedAt <- o .: ("google_created_at" :: Text)
    userResponseAcceptTOS <- o .: ("accept_tos" :: Text)
    userResponseActive <- o .: ("active" :: Text)
    userResponseGuard <- o .: ("guard" :: Text)
    userResponseCreatedAt <- o .: ("created_at" :: Text)
    userResponseModifiedAt <- o .: ("modified_at" :: Text)
    userResponseDeactivatedAt <- o .: ("deactivated_at" :: Text)
    userResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ UserResponse {
      userResponseId = userResponseId,
      userResponseName = userResponseName,
      userResponseDisplayName = userResponseDisplayName,
      userResponseFullName = userResponseFullName,
      userResponseEmail = userResponseEmail,
      userResponseEmailMD5 = userResponseEmailMD5,
      userResponsePlugin = userResponsePlugin,
      userResponseGithubIdent = userResponseGithubIdent,
      userResponseGithubCreatedAt = userResponseGithubCreatedAt,
      userResponseGoogleIdent = userResponseGoogleIdent,
      userResponseGoogleCreatedAt = userResponseGoogleCreatedAt,
      userResponseAcceptTOS = userResponseAcceptTOS,
      userResponseActive = userResponseActive,
      userResponseGuard = userResponseGuard,
      userResponseCreatedAt = userResponseCreatedAt,
      userResponseModifiedAt = userResponseModifiedAt,
      userResponseDeactivatedAt = userResponseDeactivatedAt,
      userResponseActivityAt = userResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON UserResponse where
  toJSON UserResponse{..} = object $
    [ "tag" .= ("UserResponse" :: Text)
    , "id" .= userResponseId
    , "name" .= userResponseName
    , "display_name" .= userResponseDisplayName
    , "full_name" .= userResponseFullName
    , "email" .= userResponseEmail
    , "email_md5" .= userResponseEmailMD5
    , "plugin" .= userResponsePlugin
    , "github_ident" .= userResponseGithubIdent
    , "github_created_at" .= userResponseGithubCreatedAt
    , "google_ident" .= userResponseGoogleIdent
    , "google_created_at" .= userResponseGoogleCreatedAt
    , "accept_tos" .= userResponseAcceptTOS
    , "active" .= userResponseActive
    , "guard" .= userResponseGuard
    , "created_at" .= userResponseCreatedAt
    , "modified_at" .= userResponseModifiedAt
    , "deactivated_at" .= userResponseDeactivatedAt
    , "activity_at" .= userResponseActivityAt
    ]


instance Eq UserResponse where
  (==) a b = userResponseId a == userResponseId b && userResponseName a == userResponseName b && userResponseDisplayName a == userResponseDisplayName b && userResponseFullName a == userResponseFullName b && userResponseEmail a == userResponseEmail b && userResponseEmailMD5 a == userResponseEmailMD5 b && userResponsePlugin a == userResponsePlugin b && userResponseGithubIdent a == userResponseGithubIdent b && userResponseGithubCreatedAt a == userResponseGithubCreatedAt b && userResponseGoogleIdent a == userResponseGoogleIdent b && userResponseGoogleCreatedAt a == userResponseGoogleCreatedAt b && userResponseAcceptTOS a == userResponseAcceptTOS b && userResponseActive a == userResponseActive b && userResponseGuard a == userResponseGuard b && userResponseCreatedAt a == userResponseCreatedAt b && userResponseModifiedAt a == userResponseModifiedAt b && userResponseDeactivatedAt a == userResponseDeactivatedAt b && userResponseActivityAt a == userResponseActivityAt b

instance Show UserResponse where
    show rec = "userResponseId: " <> show (userResponseId rec) <> ", " <> "userResponseName: " <> show (userResponseName rec) <> ", " <> "userResponseDisplayName: " <> show (userResponseDisplayName rec) <> ", " <> "userResponseFullName: " <> show (userResponseFullName rec) <> ", " <> "userResponseEmail: " <> show (userResponseEmail rec) <> ", " <> "userResponseEmailMD5: " <> show (userResponseEmailMD5 rec) <> ", " <> "userResponsePlugin: " <> show (userResponsePlugin rec) <> ", " <> "userResponseGithubIdent: " <> show (userResponseGithubIdent rec) <> ", " <> "userResponseGithubCreatedAt: " <> show (userResponseGithubCreatedAt rec) <> ", " <> "userResponseGoogleIdent: " <> show (userResponseGoogleIdent rec) <> ", " <> "userResponseGoogleCreatedAt: " <> show (userResponseGoogleCreatedAt rec) <> ", " <> "userResponseAcceptTOS: " <> show (userResponseAcceptTOS rec) <> ", " <> "userResponseActive: " <> show (userResponseActive rec) <> ", " <> "userResponseGuard: " <> show (userResponseGuard rec) <> ", " <> "userResponseCreatedAt: " <> show (userResponseCreatedAt rec) <> ", " <> "userResponseModifiedAt: " <> show (userResponseModifiedAt rec) <> ", " <> "userResponseDeactivatedAt: " <> show (userResponseDeactivatedAt rec) <> ", " <> "userResponseActivityAt: " <> show (userResponseActivityAt rec)

data UserResponses = UserResponses {
  userResponses :: !([UserResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON UserResponses where
  parseJSON (Object o) = do
    userResponses <- o .: ("user_responses" :: Text)
    pure $ UserResponses {
      userResponses = userResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON UserResponses where
  toJSON UserResponses{..} = object $
    [ "tag" .= ("UserResponses" :: Text)
    , "user_responses" .= userResponses
    ]


instance Eq UserResponses where
  (==) a b = userResponses a == userResponses b

instance Show UserResponses where
    show rec = "userResponses: " <> show (userResponses rec)

data UserSanitizedResponse = UserSanitizedResponse {
  userSanitizedResponseId :: !(Int64),
  userSanitizedResponseName :: !(Text),
  userSanitizedResponseDisplayName :: !(Text),
  userSanitizedResponseEmailMD5 :: !(Text),
  userSanitizedResponseActive :: !(Bool),
  userSanitizedResponseGuard :: !(Int),
  userSanitizedResponseCreatedAt :: !((Maybe UTCTime)),
  userSanitizedResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON UserSanitizedResponse where
  parseJSON (Object o) = do
    userSanitizedResponseId <- o .: ("id" :: Text)
    userSanitizedResponseName <- o .: ("name" :: Text)
    userSanitizedResponseDisplayName <- o .: ("display_name" :: Text)
    userSanitizedResponseEmailMD5 <- o .: ("email_md5" :: Text)
    userSanitizedResponseActive <- o .: ("active" :: Text)
    userSanitizedResponseGuard <- o .: ("guard" :: Text)
    userSanitizedResponseCreatedAt <- o .: ("created_at" :: Text)
    userSanitizedResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ UserSanitizedResponse {
      userSanitizedResponseId = userSanitizedResponseId,
      userSanitizedResponseName = userSanitizedResponseName,
      userSanitizedResponseDisplayName = userSanitizedResponseDisplayName,
      userSanitizedResponseEmailMD5 = userSanitizedResponseEmailMD5,
      userSanitizedResponseActive = userSanitizedResponseActive,
      userSanitizedResponseGuard = userSanitizedResponseGuard,
      userSanitizedResponseCreatedAt = userSanitizedResponseCreatedAt,
      userSanitizedResponseActivityAt = userSanitizedResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON UserSanitizedResponse where
  toJSON UserSanitizedResponse{..} = object $
    [ "tag" .= ("UserSanitizedResponse" :: Text)
    , "id" .= userSanitizedResponseId
    , "name" .= userSanitizedResponseName
    , "display_name" .= userSanitizedResponseDisplayName
    , "email_md5" .= userSanitizedResponseEmailMD5
    , "active" .= userSanitizedResponseActive
    , "guard" .= userSanitizedResponseGuard
    , "created_at" .= userSanitizedResponseCreatedAt
    , "activity_at" .= userSanitizedResponseActivityAt
    ]


instance Eq UserSanitizedResponse where
  (==) a b = userSanitizedResponseId a == userSanitizedResponseId b && userSanitizedResponseName a == userSanitizedResponseName b && userSanitizedResponseDisplayName a == userSanitizedResponseDisplayName b && userSanitizedResponseEmailMD5 a == userSanitizedResponseEmailMD5 b && userSanitizedResponseActive a == userSanitizedResponseActive b && userSanitizedResponseGuard a == userSanitizedResponseGuard b && userSanitizedResponseCreatedAt a == userSanitizedResponseCreatedAt b && userSanitizedResponseActivityAt a == userSanitizedResponseActivityAt b

instance Show UserSanitizedResponse where
    show rec = "userSanitizedResponseId: " <> show (userSanitizedResponseId rec) <> ", " <> "userSanitizedResponseName: " <> show (userSanitizedResponseName rec) <> ", " <> "userSanitizedResponseDisplayName: " <> show (userSanitizedResponseDisplayName rec) <> ", " <> "userSanitizedResponseEmailMD5: " <> show (userSanitizedResponseEmailMD5 rec) <> ", " <> "userSanitizedResponseActive: " <> show (userSanitizedResponseActive rec) <> ", " <> "userSanitizedResponseGuard: " <> show (userSanitizedResponseGuard rec) <> ", " <> "userSanitizedResponseCreatedAt: " <> show (userSanitizedResponseCreatedAt rec) <> ", " <> "userSanitizedResponseActivityAt: " <> show (userSanitizedResponseActivityAt rec)

data UserSanitizedResponses = UserSanitizedResponses {
  userSanitizedResponses :: !([UserSanitizedResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON UserSanitizedResponses where
  parseJSON (Object o) = do
    userSanitizedResponses <- o .: ("user_sanitized_responses" :: Text)
    pure $ UserSanitizedResponses {
      userSanitizedResponses = userSanitizedResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON UserSanitizedResponses where
  toJSON UserSanitizedResponses{..} = object $
    [ "tag" .= ("UserSanitizedResponses" :: Text)
    , "user_sanitized_responses" .= userSanitizedResponses
    ]


instance Eq UserSanitizedResponses where
  (==) a b = userSanitizedResponses a == userSanitizedResponses b

instance Show UserSanitizedResponses where
    show rec = "userSanitizedResponses: " <> show (userSanitizedResponses rec)

data UserSanitizedStatResponse = UserSanitizedStatResponse {
  userSanitizedStatResponseUserId :: !(Int64),
  userSanitizedStatResponseThreads :: !(Int64),
  userSanitizedStatResponseThreadPosts :: !(Int64),
  userSanitizedStatResponseRespect :: !(Int64),
  userSanitizedStatResponseWorkouts :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON UserSanitizedStatResponse where
  parseJSON (Object o) = do
    userSanitizedStatResponseUserId <- o .: ("user_id" :: Text)
    userSanitizedStatResponseThreads <- o .: ("threads" :: Text)
    userSanitizedStatResponseThreadPosts <- o .: ("thread_posts" :: Text)
    userSanitizedStatResponseRespect <- o .: ("respect" :: Text)
    userSanitizedStatResponseWorkouts <- o .: ("workouts" :: Text)
    pure $ UserSanitizedStatResponse {
      userSanitizedStatResponseUserId = userSanitizedStatResponseUserId,
      userSanitizedStatResponseThreads = userSanitizedStatResponseThreads,
      userSanitizedStatResponseThreadPosts = userSanitizedStatResponseThreadPosts,
      userSanitizedStatResponseRespect = userSanitizedStatResponseRespect,
      userSanitizedStatResponseWorkouts = userSanitizedStatResponseWorkouts
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON UserSanitizedStatResponse where
  toJSON UserSanitizedStatResponse{..} = object $
    [ "tag" .= ("UserSanitizedStatResponse" :: Text)
    , "user_id" .= userSanitizedStatResponseUserId
    , "threads" .= userSanitizedStatResponseThreads
    , "thread_posts" .= userSanitizedStatResponseThreadPosts
    , "respect" .= userSanitizedStatResponseRespect
    , "workouts" .= userSanitizedStatResponseWorkouts
    ]


instance Eq UserSanitizedStatResponse where
  (==) a b = userSanitizedStatResponseUserId a == userSanitizedStatResponseUserId b && userSanitizedStatResponseThreads a == userSanitizedStatResponseThreads b && userSanitizedStatResponseThreadPosts a == userSanitizedStatResponseThreadPosts b && userSanitizedStatResponseRespect a == userSanitizedStatResponseRespect b && userSanitizedStatResponseWorkouts a == userSanitizedStatResponseWorkouts b

instance Show UserSanitizedStatResponse where
    show rec = "userSanitizedStatResponseUserId: " <> show (userSanitizedStatResponseUserId rec) <> ", " <> "userSanitizedStatResponseThreads: " <> show (userSanitizedStatResponseThreads rec) <> ", " <> "userSanitizedStatResponseThreadPosts: " <> show (userSanitizedStatResponseThreadPosts rec) <> ", " <> "userSanitizedStatResponseRespect: " <> show (userSanitizedStatResponseRespect rec) <> ", " <> "userSanitizedStatResponseWorkouts: " <> show (userSanitizedStatResponseWorkouts rec)

data UserSanitizedStatResponses = UserSanitizedStatResponses {
  userSanitizedStatResponses :: !([UserSanitizedStatResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON UserSanitizedStatResponses where
  parseJSON (Object o) = do
    userSanitizedStatResponses <- o .: ("user_sanitized_stat_responses" :: Text)
    pure $ UserSanitizedStatResponses {
      userSanitizedStatResponses = userSanitizedStatResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON UserSanitizedStatResponses where
  toJSON UserSanitizedStatResponses{..} = object $
    [ "tag" .= ("UserSanitizedStatResponses" :: Text)
    , "user_sanitized_stat_responses" .= userSanitizedStatResponses
    ]


instance Eq UserSanitizedStatResponses where
  (==) a b = userSanitizedStatResponses a == userSanitizedStatResponses b

instance Show UserSanitizedStatResponses where
    show rec = "userSanitizedStatResponses: " <> show (userSanitizedStatResponses rec)
-- footer