{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.GlobalGroup where


import LN.T.Membership
import LN.T.Visibility


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

data GlobalGroupRequest = GlobalGroupRequest {
  globalGroupRequestDisplayName :: !(Text),
  globalGroupRequestDescription :: !((Maybe Text)),
  globalGroupRequestMembership :: !(Membership),
  globalGroupRequestIcon :: !((Maybe Text)),
  globalGroupRequestTags :: !([Text]),
  globalGroupRequestVisibility :: !(Visibility),
  globalGroupRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON GlobalGroupRequest where
  parseJSON (Object o) = do
    globalGroupRequestDisplayName <- o .: ("display_name" :: Text)
    globalGroupRequestDescription <- o .: ("description" :: Text)
    globalGroupRequestMembership <- o .: ("membership" :: Text)
    globalGroupRequestIcon <- o .: ("icon" :: Text)
    globalGroupRequestTags <- o .: ("tags" :: Text)
    globalGroupRequestVisibility <- o .: ("visibility" :: Text)
    globalGroupRequestGuard <- o .: ("guard" :: Text)
    pure $ GlobalGroupRequest {
      globalGroupRequestDisplayName = globalGroupRequestDisplayName,
      globalGroupRequestDescription = globalGroupRequestDescription,
      globalGroupRequestMembership = globalGroupRequestMembership,
      globalGroupRequestIcon = globalGroupRequestIcon,
      globalGroupRequestTags = globalGroupRequestTags,
      globalGroupRequestVisibility = globalGroupRequestVisibility,
      globalGroupRequestGuard = globalGroupRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GlobalGroupRequest where
  toJSON GlobalGroupRequest{..} = object $
    [ "tag" .= ("GlobalGroupRequest" :: Text)
    , "display_name" .= globalGroupRequestDisplayName
    , "description" .= globalGroupRequestDescription
    , "membership" .= globalGroupRequestMembership
    , "icon" .= globalGroupRequestIcon
    , "tags" .= globalGroupRequestTags
    , "visibility" .= globalGroupRequestVisibility
    , "guard" .= globalGroupRequestGuard
    ]


instance Eq GlobalGroupRequest where
  (==) a b = globalGroupRequestDisplayName a == globalGroupRequestDisplayName b && globalGroupRequestDescription a == globalGroupRequestDescription b && globalGroupRequestMembership a == globalGroupRequestMembership b && globalGroupRequestIcon a == globalGroupRequestIcon b && globalGroupRequestTags a == globalGroupRequestTags b && globalGroupRequestVisibility a == globalGroupRequestVisibility b && globalGroupRequestGuard a == globalGroupRequestGuard b

instance Show GlobalGroupRequest where
    show rec = "globalGroupRequestDisplayName: " <> show (globalGroupRequestDisplayName rec) <> ", " <> "globalGroupRequestDescription: " <> show (globalGroupRequestDescription rec) <> ", " <> "globalGroupRequestMembership: " <> show (globalGroupRequestMembership rec) <> ", " <> "globalGroupRequestIcon: " <> show (globalGroupRequestIcon rec) <> ", " <> "globalGroupRequestTags: " <> show (globalGroupRequestTags rec) <> ", " <> "globalGroupRequestVisibility: " <> show (globalGroupRequestVisibility rec) <> ", " <> "globalGroupRequestGuard: " <> show (globalGroupRequestGuard rec)

data GlobalGroupResponse = GlobalGroupResponse {
  globalGroupResponseId :: !(Int64),
  globalGroupResponseUserId :: !(Int64),
  globalGroupResponseName :: !(Text),
  globalGroupResponseDisplayName :: !(Text),
  globalGroupResponseDescription :: !((Maybe Text)),
  globalGroupResponseMembership :: !(Membership),
  globalGroupResponseIcon :: !((Maybe Text)),
  globalGroupResponseTags :: !([Text]),
  globalGroupResponseVisibility :: !(Visibility),
  globalGroupResponseActive :: !(Bool),
  globalGroupResponseGuard :: !(Int),
  globalGroupResponseCreatedAt :: !((Maybe UTCTime)),
  globalGroupResponseModifiedBy :: !((Maybe Int64)),
  globalGroupResponseModifiedAt :: !((Maybe UTCTime)),
  globalGroupResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON GlobalGroupResponse where
  parseJSON (Object o) = do
    globalGroupResponseId <- o .: ("id" :: Text)
    globalGroupResponseUserId <- o .: ("user_id" :: Text)
    globalGroupResponseName <- o .: ("name" :: Text)
    globalGroupResponseDisplayName <- o .: ("display_name" :: Text)
    globalGroupResponseDescription <- o .: ("description" :: Text)
    globalGroupResponseMembership <- o .: ("membership" :: Text)
    globalGroupResponseIcon <- o .: ("icon" :: Text)
    globalGroupResponseTags <- o .: ("tags" :: Text)
    globalGroupResponseVisibility <- o .: ("visibility" :: Text)
    globalGroupResponseActive <- o .: ("active" :: Text)
    globalGroupResponseGuard <- o .: ("guard" :: Text)
    globalGroupResponseCreatedAt <- o .: ("created_at" :: Text)
    globalGroupResponseModifiedBy <- o .: ("modified_by" :: Text)
    globalGroupResponseModifiedAt <- o .: ("modified_at" :: Text)
    globalGroupResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ GlobalGroupResponse {
      globalGroupResponseId = globalGroupResponseId,
      globalGroupResponseUserId = globalGroupResponseUserId,
      globalGroupResponseName = globalGroupResponseName,
      globalGroupResponseDisplayName = globalGroupResponseDisplayName,
      globalGroupResponseDescription = globalGroupResponseDescription,
      globalGroupResponseMembership = globalGroupResponseMembership,
      globalGroupResponseIcon = globalGroupResponseIcon,
      globalGroupResponseTags = globalGroupResponseTags,
      globalGroupResponseVisibility = globalGroupResponseVisibility,
      globalGroupResponseActive = globalGroupResponseActive,
      globalGroupResponseGuard = globalGroupResponseGuard,
      globalGroupResponseCreatedAt = globalGroupResponseCreatedAt,
      globalGroupResponseModifiedBy = globalGroupResponseModifiedBy,
      globalGroupResponseModifiedAt = globalGroupResponseModifiedAt,
      globalGroupResponseActivityAt = globalGroupResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GlobalGroupResponse where
  toJSON GlobalGroupResponse{..} = object $
    [ "tag" .= ("GlobalGroupResponse" :: Text)
    , "id" .= globalGroupResponseId
    , "user_id" .= globalGroupResponseUserId
    , "name" .= globalGroupResponseName
    , "display_name" .= globalGroupResponseDisplayName
    , "description" .= globalGroupResponseDescription
    , "membership" .= globalGroupResponseMembership
    , "icon" .= globalGroupResponseIcon
    , "tags" .= globalGroupResponseTags
    , "visibility" .= globalGroupResponseVisibility
    , "active" .= globalGroupResponseActive
    , "guard" .= globalGroupResponseGuard
    , "created_at" .= globalGroupResponseCreatedAt
    , "modified_by" .= globalGroupResponseModifiedBy
    , "modified_at" .= globalGroupResponseModifiedAt
    , "activity_at" .= globalGroupResponseActivityAt
    ]


instance Eq GlobalGroupResponse where
  (==) a b = globalGroupResponseId a == globalGroupResponseId b && globalGroupResponseUserId a == globalGroupResponseUserId b && globalGroupResponseName a == globalGroupResponseName b && globalGroupResponseDisplayName a == globalGroupResponseDisplayName b && globalGroupResponseDescription a == globalGroupResponseDescription b && globalGroupResponseMembership a == globalGroupResponseMembership b && globalGroupResponseIcon a == globalGroupResponseIcon b && globalGroupResponseTags a == globalGroupResponseTags b && globalGroupResponseVisibility a == globalGroupResponseVisibility b && globalGroupResponseActive a == globalGroupResponseActive b && globalGroupResponseGuard a == globalGroupResponseGuard b && globalGroupResponseCreatedAt a == globalGroupResponseCreatedAt b && globalGroupResponseModifiedBy a == globalGroupResponseModifiedBy b && globalGroupResponseModifiedAt a == globalGroupResponseModifiedAt b && globalGroupResponseActivityAt a == globalGroupResponseActivityAt b

instance Show GlobalGroupResponse where
    show rec = "globalGroupResponseId: " <> show (globalGroupResponseId rec) <> ", " <> "globalGroupResponseUserId: " <> show (globalGroupResponseUserId rec) <> ", " <> "globalGroupResponseName: " <> show (globalGroupResponseName rec) <> ", " <> "globalGroupResponseDisplayName: " <> show (globalGroupResponseDisplayName rec) <> ", " <> "globalGroupResponseDescription: " <> show (globalGroupResponseDescription rec) <> ", " <> "globalGroupResponseMembership: " <> show (globalGroupResponseMembership rec) <> ", " <> "globalGroupResponseIcon: " <> show (globalGroupResponseIcon rec) <> ", " <> "globalGroupResponseTags: " <> show (globalGroupResponseTags rec) <> ", " <> "globalGroupResponseVisibility: " <> show (globalGroupResponseVisibility rec) <> ", " <> "globalGroupResponseActive: " <> show (globalGroupResponseActive rec) <> ", " <> "globalGroupResponseGuard: " <> show (globalGroupResponseGuard rec) <> ", " <> "globalGroupResponseCreatedAt: " <> show (globalGroupResponseCreatedAt rec) <> ", " <> "globalGroupResponseModifiedBy: " <> show (globalGroupResponseModifiedBy rec) <> ", " <> "globalGroupResponseModifiedAt: " <> show (globalGroupResponseModifiedAt rec) <> ", " <> "globalGroupResponseActivityAt: " <> show (globalGroupResponseActivityAt rec)

data GlobalGroupResponses = GlobalGroupResponses {
  globalGroupResponses :: !([GlobalGroupResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON GlobalGroupResponses where
  parseJSON (Object o) = do
    globalGroupResponses <- o .: ("global_group_responses" :: Text)
    pure $ GlobalGroupResponses {
      globalGroupResponses = globalGroupResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GlobalGroupResponses where
  toJSON GlobalGroupResponses{..} = object $
    [ "tag" .= ("GlobalGroupResponses" :: Text)
    , "global_group_responses" .= globalGroupResponses
    ]


instance Eq GlobalGroupResponses where
  (==) a b = globalGroupResponses a == globalGroupResponses b

instance Show GlobalGroupResponses where
    show rec = "globalGroupResponses: " <> show (globalGroupResponses rec)

data GlobalGroupStatResponse = GlobalGroupStatResponse {
  globalGroupStatResponseGroups :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON GlobalGroupStatResponse where
  parseJSON (Object o) = do
    globalGroupStatResponseGroups <- o .: ("groups" :: Text)
    pure $ GlobalGroupStatResponse {
      globalGroupStatResponseGroups = globalGroupStatResponseGroups
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GlobalGroupStatResponse where
  toJSON GlobalGroupStatResponse{..} = object $
    [ "tag" .= ("GlobalGroupStatResponse" :: Text)
    , "groups" .= globalGroupStatResponseGroups
    ]


instance Eq GlobalGroupStatResponse where
  (==) a b = globalGroupStatResponseGroups a == globalGroupStatResponseGroups b

instance Show GlobalGroupStatResponse where
    show rec = "globalGroupStatResponseGroups: " <> show (globalGroupStatResponseGroups rec)

data GlobalGroupStatResponses = GlobalGroupStatResponses {
  globalGroupStatResponses :: !([GlobalGroupStatResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON GlobalGroupStatResponses where
  parseJSON (Object o) = do
    globalGroupStatResponses <- o .: ("global_group_stat_responses" :: Text)
    pure $ GlobalGroupStatResponses {
      globalGroupStatResponses = globalGroupStatResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GlobalGroupStatResponses where
  toJSON GlobalGroupStatResponses{..} = object $
    [ "tag" .= ("GlobalGroupStatResponses" :: Text)
    , "global_group_stat_responses" .= globalGroupStatResponses
    ]


instance Eq GlobalGroupStatResponses where
  (==) a b = globalGroupStatResponses a == globalGroupStatResponses b

instance Show GlobalGroupStatResponses where
    show rec = "globalGroupStatResponses: " <> show (globalGroupStatResponses rec)
-- footer