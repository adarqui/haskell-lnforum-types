{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Group where





import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Int            (Int64)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)
import           Data.Default

data GroupRequest = GroupRequest {
  groupRequestGuard :: Int
}


instance FromJSON GroupRequest where
  parseJSON (Object o) = do
    groupRequestGuard <- o .: ("guard" :: Text)
    pure $ GroupRequest {
      groupRequestGuard = groupRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GroupRequest where
  toJSON GroupRequest{..} = object $
    [ "tag" .= ("GroupRequest" :: Text)
    , "guard" .= groupRequestGuard
    ]


instance Eq GroupRequest where
  (==) a b = groupRequestGuard a == groupRequestGuard b

instance Show GroupRequest where
    show rec = "groupRequestGuard: " <> show (groupRequestGuard rec)

data GroupResponse = GroupResponse {
  groupResponseId :: Int64,
  groupResponseUserId :: Int64,
  groupResponseGlobalGroupId :: Int64,
  groupResponseOrganizationId :: Int64,
  groupResponseActive :: Bool,
  groupResponseGuard :: Int,
  groupResponseCreatedAt :: (Maybe UTCTime),
  groupResponseModifiedBy :: (Maybe Int64),
  groupResponseModifiedAt :: (Maybe UTCTime),
  groupResponseActivityAt :: (Maybe UTCTime)
}


instance FromJSON GroupResponse where
  parseJSON (Object o) = do
    groupResponseId <- o .: ("id" :: Text)
    groupResponseUserId <- o .: ("user_id" :: Text)
    groupResponseGlobalGroupId <- o .: ("global_group_id" :: Text)
    groupResponseOrganizationId <- o .: ("organization_id" :: Text)
    groupResponseActive <- o .: ("active" :: Text)
    groupResponseGuard <- o .: ("guard" :: Text)
    groupResponseCreatedAt <- o .: ("created_at" :: Text)
    groupResponseModifiedBy <- o .: ("modified_by" :: Text)
    groupResponseModifiedAt <- o .: ("modified_at" :: Text)
    groupResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ GroupResponse {
      groupResponseId = groupResponseId,
      groupResponseUserId = groupResponseUserId,
      groupResponseGlobalGroupId = groupResponseGlobalGroupId,
      groupResponseOrganizationId = groupResponseOrganizationId,
      groupResponseActive = groupResponseActive,
      groupResponseGuard = groupResponseGuard,
      groupResponseCreatedAt = groupResponseCreatedAt,
      groupResponseModifiedBy = groupResponseModifiedBy,
      groupResponseModifiedAt = groupResponseModifiedAt,
      groupResponseActivityAt = groupResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GroupResponse where
  toJSON GroupResponse{..} = object $
    [ "tag" .= ("GroupResponse" :: Text)
    , "id" .= groupResponseId
    , "user_id" .= groupResponseUserId
    , "global_group_id" .= groupResponseGlobalGroupId
    , "organization_id" .= groupResponseOrganizationId
    , "active" .= groupResponseActive
    , "guard" .= groupResponseGuard
    , "created_at" .= groupResponseCreatedAt
    , "modified_by" .= groupResponseModifiedBy
    , "modified_at" .= groupResponseModifiedAt
    , "activity_at" .= groupResponseActivityAt
    ]


instance Eq GroupResponse where
  (==) a b = groupResponseId a == groupResponseId b && groupResponseUserId a == groupResponseUserId b && groupResponseGlobalGroupId a == groupResponseGlobalGroupId b && groupResponseOrganizationId a == groupResponseOrganizationId b && groupResponseActive a == groupResponseActive b && groupResponseGuard a == groupResponseGuard b && groupResponseCreatedAt a == groupResponseCreatedAt b && groupResponseModifiedBy a == groupResponseModifiedBy b && groupResponseModifiedAt a == groupResponseModifiedAt b && groupResponseActivityAt a == groupResponseActivityAt b

instance Show GroupResponse where
    show rec = "groupResponseId: " <> show (groupResponseId rec) <> ", " <> "groupResponseUserId: " <> show (groupResponseUserId rec) <> ", " <> "groupResponseGlobalGroupId: " <> show (groupResponseGlobalGroupId rec) <> ", " <> "groupResponseOrganizationId: " <> show (groupResponseOrganizationId rec) <> ", " <> "groupResponseActive: " <> show (groupResponseActive rec) <> ", " <> "groupResponseGuard: " <> show (groupResponseGuard rec) <> ", " <> "groupResponseCreatedAt: " <> show (groupResponseCreatedAt rec) <> ", " <> "groupResponseModifiedBy: " <> show (groupResponseModifiedBy rec) <> ", " <> "groupResponseModifiedAt: " <> show (groupResponseModifiedAt rec) <> ", " <> "groupResponseActivityAt: " <> show (groupResponseActivityAt rec)

data GroupResponses = GroupResponses {
  groupResponses :: [GroupResponse]
}


instance FromJSON GroupResponses where
  parseJSON (Object o) = do
    groupResponses <- o .: ("group_responses" :: Text)
    pure $ GroupResponses {
      groupResponses = groupResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GroupResponses where
  toJSON GroupResponses{..} = object $
    [ "tag" .= ("GroupResponses" :: Text)
    , "group_responses" .= groupResponses
    ]


instance Eq GroupResponses where
  (==) a b = groupResponses a == groupResponses b

instance Show GroupResponses where
    show rec = "groupResponses: " <> show (groupResponses rec)

data GroupStatResponse = GroupStatResponse {
  groupStatResponseMembers :: Int64
}


instance FromJSON GroupStatResponse where
  parseJSON (Object o) = do
    groupStatResponseMembers <- o .: ("members" :: Text)
    pure $ GroupStatResponse {
      groupStatResponseMembers = groupStatResponseMembers
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GroupStatResponse where
  toJSON GroupStatResponse{..} = object $
    [ "tag" .= ("GroupStatResponse" :: Text)
    , "members" .= groupStatResponseMembers
    ]


instance Eq GroupStatResponse where
  (==) a b = groupStatResponseMembers a == groupStatResponseMembers b

instance Show GroupStatResponse where
    show rec = "groupStatResponseMembers: " <> show (groupStatResponseMembers rec)

data GroupStatResponses = GroupStatResponses {
  groupStatResponses :: [GroupStatResponse]
}


instance FromJSON GroupStatResponses where
  parseJSON (Object o) = do
    groupStatResponses <- o .: ("group_stat_responses" :: Text)
    pure $ GroupStatResponses {
      groupStatResponses = groupStatResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GroupStatResponses where
  toJSON GroupStatResponses{..} = object $
    [ "tag" .= ("GroupStatResponses" :: Text)
    , "group_stat_responses" .= groupStatResponses
    ]


instance Eq GroupStatResponses where
  (==) a b = groupStatResponses a == groupStatResponses b

instance Show GroupStatResponses where
    show rec = "groupStatResponses: " <> show (groupStatResponses rec)
-- footer