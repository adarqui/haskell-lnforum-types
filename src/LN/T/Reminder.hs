{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Reminder where


import LN.T.Visibility


import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Int            (Int64)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)

data ReminderRequest = ReminderRequest {
  reminderRequestData :: Text,
  reminderRequestGuard :: Int
}


instance FromJSON ReminderRequest where
  parseJSON (Object o) = do
    reminderRequestData <- o .: ("data" :: Text)
    reminderRequestGuard <- o .: ("guard" :: Text)
    pure $ ReminderRequest {
      reminderRequestData = reminderRequestData,
      reminderRequestGuard = reminderRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ReminderRequest where
  toJSON ReminderRequest{..} = object $
    [ "tag" .= ("ReminderRequest" :: Text)
    , "data" .= reminderRequestData
    , "guard" .= reminderRequestGuard
    ]


instance Eq ReminderRequest where
  (==) a b = reminderRequestData a == reminderRequestData b && reminderRequestGuard a == reminderRequestGuard b

instance Show ReminderRequest where
    show rec = "reminderRequestData: " <> show (reminderRequestData rec) <> ", " <> "reminderRequestGuard: " <> show (reminderRequestGuard rec)

data ReminderResponse = ReminderResponse {
  reminderResponseId :: Int64,
  reminderResponseUserId :: Int64,
  reminderResponseParentFolderId :: Int64,
  reminderResponseData :: Text,
  reminderResponseActive :: Bool,
  reminderResponseGuard :: Int,
  reminderResponseCreatedAt :: (Maybe UTCTime),
  reminderResponseModifiedAt :: (Maybe UTCTime),
  reminderResponseActivityAt :: (Maybe UTCTime)
}


instance FromJSON ReminderResponse where
  parseJSON (Object o) = do
    reminderResponseId <- o .: ("id" :: Text)
    reminderResponseUserId <- o .: ("user_id" :: Text)
    reminderResponseParentFolderId <- o .: ("parent_folder_id" :: Text)
    reminderResponseData <- o .: ("data" :: Text)
    reminderResponseActive <- o .: ("active" :: Text)
    reminderResponseGuard <- o .: ("guard" :: Text)
    reminderResponseCreatedAt <- o .: ("created_at" :: Text)
    reminderResponseModifiedAt <- o .: ("modified_at" :: Text)
    reminderResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ ReminderResponse {
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
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ReminderResponse where
  toJSON ReminderResponse{..} = object $
    [ "tag" .= ("ReminderResponse" :: Text)
    , "id" .= reminderResponseId
    , "user_id" .= reminderResponseUserId
    , "parent_folder_id" .= reminderResponseParentFolderId
    , "data" .= reminderResponseData
    , "active" .= reminderResponseActive
    , "guard" .= reminderResponseGuard
    , "created_at" .= reminderResponseCreatedAt
    , "modified_at" .= reminderResponseModifiedAt
    , "activity_at" .= reminderResponseActivityAt
    ]


instance Eq ReminderResponse where
  (==) a b = reminderResponseId a == reminderResponseId b && reminderResponseUserId a == reminderResponseUserId b && reminderResponseParentFolderId a == reminderResponseParentFolderId b && reminderResponseData a == reminderResponseData b && reminderResponseActive a == reminderResponseActive b && reminderResponseGuard a == reminderResponseGuard b && reminderResponseCreatedAt a == reminderResponseCreatedAt b && reminderResponseModifiedAt a == reminderResponseModifiedAt b && reminderResponseActivityAt a == reminderResponseActivityAt b

instance Show ReminderResponse where
    show rec = "reminderResponseId: " <> show (reminderResponseId rec) <> ", " <> "reminderResponseUserId: " <> show (reminderResponseUserId rec) <> ", " <> "reminderResponseParentFolderId: " <> show (reminderResponseParentFolderId rec) <> ", " <> "reminderResponseData: " <> show (reminderResponseData rec) <> ", " <> "reminderResponseActive: " <> show (reminderResponseActive rec) <> ", " <> "reminderResponseGuard: " <> show (reminderResponseGuard rec) <> ", " <> "reminderResponseCreatedAt: " <> show (reminderResponseCreatedAt rec) <> ", " <> "reminderResponseModifiedAt: " <> show (reminderResponseModifiedAt rec) <> ", " <> "reminderResponseActivityAt: " <> show (reminderResponseActivityAt rec)

data ReminderResponses = ReminderResponses {
  reminderResponses :: [ReminderResponse]
}


instance FromJSON ReminderResponses where
  parseJSON (Object o) = do
    reminderResponses <- o .: ("reminder_responses" :: Text)
    pure $ ReminderResponses {
      reminderResponses = reminderResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ReminderResponses where
  toJSON ReminderResponses{..} = object $
    [ "tag" .= ("ReminderResponses" :: Text)
    , "reminder_responses" .= reminderResponses
    ]


instance Eq ReminderResponses where
  (==) a b = reminderResponses a == reminderResponses b

instance Show ReminderResponses where
    show rec = "reminderResponses: " <> show (reminderResponses rec)

data ReminderFolderRequest = ReminderFolderRequest {
  reminderFolderRequestDisplayName :: Text,
  reminderFolderRequestDescription :: (Maybe Text),
  reminderFolderRequestVisibility :: Visibility,
  reminderFolderRequestGuard :: Int
}


instance FromJSON ReminderFolderRequest where
  parseJSON (Object o) = do
    reminderFolderRequestDisplayName <- o .: ("display_name" :: Text)
    reminderFolderRequestDescription <- o .: ("description" :: Text)
    reminderFolderRequestVisibility <- o .: ("visibility" :: Text)
    reminderFolderRequestGuard <- o .: ("guard" :: Text)
    pure $ ReminderFolderRequest {
      reminderFolderRequestDisplayName = reminderFolderRequestDisplayName,
      reminderFolderRequestDescription = reminderFolderRequestDescription,
      reminderFolderRequestVisibility = reminderFolderRequestVisibility,
      reminderFolderRequestGuard = reminderFolderRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ReminderFolderRequest where
  toJSON ReminderFolderRequest{..} = object $
    [ "tag" .= ("ReminderFolderRequest" :: Text)
    , "display_name" .= reminderFolderRequestDisplayName
    , "description" .= reminderFolderRequestDescription
    , "visibility" .= reminderFolderRequestVisibility
    , "guard" .= reminderFolderRequestGuard
    ]


instance Eq ReminderFolderRequest where
  (==) a b = reminderFolderRequestDisplayName a == reminderFolderRequestDisplayName b && reminderFolderRequestDescription a == reminderFolderRequestDescription b && reminderFolderRequestVisibility a == reminderFolderRequestVisibility b && reminderFolderRequestGuard a == reminderFolderRequestGuard b

instance Show ReminderFolderRequest where
    show rec = "reminderFolderRequestDisplayName: " <> show (reminderFolderRequestDisplayName rec) <> ", " <> "reminderFolderRequestDescription: " <> show (reminderFolderRequestDescription rec) <> ", " <> "reminderFolderRequestVisibility: " <> show (reminderFolderRequestVisibility rec) <> ", " <> "reminderFolderRequestGuard: " <> show (reminderFolderRequestGuard rec)

data ReminderFolderResponse = ReminderFolderResponse {
  reminderFolderResponseId :: Int64,
  reminderFolderResponseUserId :: Int64,
  reminderFolderResponseParentFolderId :: (Maybe Int64),
  reminderFolderResponseName :: Text,
  reminderFolderResponseDisplayName :: Text,
  reminderFolderResponseVisibility :: Visibility,
  reminderFolderResponseDescription :: (Maybe Text),
  reminderFolderResponseActive :: Bool,
  reminderFolderResponseGuard :: Int,
  reminderFolderResponseCreatedAt :: (Maybe UTCTime),
  reminderFolderResponseModifiedAt :: (Maybe UTCTime),
  reminderFolderResponseActivityAt :: (Maybe UTCTime)
}


instance FromJSON ReminderFolderResponse where
  parseJSON (Object o) = do
    reminderFolderResponseId <- o .: ("id" :: Text)
    reminderFolderResponseUserId <- o .: ("user_id" :: Text)
    reminderFolderResponseParentFolderId <- o .: ("parent_folder_id" :: Text)
    reminderFolderResponseName <- o .: ("name" :: Text)
    reminderFolderResponseDisplayName <- o .: ("display_name" :: Text)
    reminderFolderResponseVisibility <- o .: ("visibility" :: Text)
    reminderFolderResponseDescription <- o .: ("description" :: Text)
    reminderFolderResponseActive <- o .: ("active" :: Text)
    reminderFolderResponseGuard <- o .: ("guard" :: Text)
    reminderFolderResponseCreatedAt <- o .: ("created_at" :: Text)
    reminderFolderResponseModifiedAt <- o .: ("modified_at" :: Text)
    reminderFolderResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ ReminderFolderResponse {
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
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ReminderFolderResponse where
  toJSON ReminderFolderResponse{..} = object $
    [ "tag" .= ("ReminderFolderResponse" :: Text)
    , "id" .= reminderFolderResponseId
    , "user_id" .= reminderFolderResponseUserId
    , "parent_folder_id" .= reminderFolderResponseParentFolderId
    , "name" .= reminderFolderResponseName
    , "display_name" .= reminderFolderResponseDisplayName
    , "visibility" .= reminderFolderResponseVisibility
    , "description" .= reminderFolderResponseDescription
    , "active" .= reminderFolderResponseActive
    , "guard" .= reminderFolderResponseGuard
    , "created_at" .= reminderFolderResponseCreatedAt
    , "modified_at" .= reminderFolderResponseModifiedAt
    , "activity_at" .= reminderFolderResponseActivityAt
    ]


instance Eq ReminderFolderResponse where
  (==) a b = reminderFolderResponseId a == reminderFolderResponseId b && reminderFolderResponseUserId a == reminderFolderResponseUserId b && reminderFolderResponseParentFolderId a == reminderFolderResponseParentFolderId b && reminderFolderResponseName a == reminderFolderResponseName b && reminderFolderResponseDisplayName a == reminderFolderResponseDisplayName b && reminderFolderResponseVisibility a == reminderFolderResponseVisibility b && reminderFolderResponseDescription a == reminderFolderResponseDescription b && reminderFolderResponseActive a == reminderFolderResponseActive b && reminderFolderResponseGuard a == reminderFolderResponseGuard b && reminderFolderResponseCreatedAt a == reminderFolderResponseCreatedAt b && reminderFolderResponseModifiedAt a == reminderFolderResponseModifiedAt b && reminderFolderResponseActivityAt a == reminderFolderResponseActivityAt b

instance Show ReminderFolderResponse where
    show rec = "reminderFolderResponseId: " <> show (reminderFolderResponseId rec) <> ", " <> "reminderFolderResponseUserId: " <> show (reminderFolderResponseUserId rec) <> ", " <> "reminderFolderResponseParentFolderId: " <> show (reminderFolderResponseParentFolderId rec) <> ", " <> "reminderFolderResponseName: " <> show (reminderFolderResponseName rec) <> ", " <> "reminderFolderResponseDisplayName: " <> show (reminderFolderResponseDisplayName rec) <> ", " <> "reminderFolderResponseVisibility: " <> show (reminderFolderResponseVisibility rec) <> ", " <> "reminderFolderResponseDescription: " <> show (reminderFolderResponseDescription rec) <> ", " <> "reminderFolderResponseActive: " <> show (reminderFolderResponseActive rec) <> ", " <> "reminderFolderResponseGuard: " <> show (reminderFolderResponseGuard rec) <> ", " <> "reminderFolderResponseCreatedAt: " <> show (reminderFolderResponseCreatedAt rec) <> ", " <> "reminderFolderResponseModifiedAt: " <> show (reminderFolderResponseModifiedAt rec) <> ", " <> "reminderFolderResponseActivityAt: " <> show (reminderFolderResponseActivityAt rec)

data ReminderFolderResponses = ReminderFolderResponses {
  reminderFolderResponses :: [ReminderFolderResponse]
}


instance FromJSON ReminderFolderResponses where
  parseJSON (Object o) = do
    reminderFolderResponses <- o .: ("reminder_folder_responses" :: Text)
    pure $ ReminderFolderResponses {
      reminderFolderResponses = reminderFolderResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ReminderFolderResponses where
  toJSON ReminderFolderResponses{..} = object $
    [ "tag" .= ("ReminderFolderResponses" :: Text)
    , "reminder_folder_responses" .= reminderFolderResponses
    ]


instance Eq ReminderFolderResponses where
  (==) a b = reminderFolderResponses a == reminderFolderResponses b

instance Show ReminderFolderResponses where
    show rec = "reminderFolderResponses: " <> show (reminderFolderResponses rec)
-- footer