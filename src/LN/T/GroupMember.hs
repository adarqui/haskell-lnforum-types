{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.GroupMember where





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

data GroupMemberRequest = GroupMemberRequest {
  groupMemberRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON GroupMemberRequest where
  parseJSON (Object o) = do
    groupMemberRequestGuard <- o .: ("guard" :: Text)
    pure $ GroupMemberRequest {
      groupMemberRequestGuard = groupMemberRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GroupMemberRequest where
  toJSON GroupMemberRequest{..} = object $
    [ "tag" .= ("GroupMemberRequest" :: Text)
    , "guard" .= groupMemberRequestGuard
    ]


instance Eq GroupMemberRequest where
  (==) a b = groupMemberRequestGuard a == groupMemberRequestGuard b

instance Show GroupMemberRequest where
    show rec = "groupMemberRequestGuard: " <> show (groupMemberRequestGuard rec)

data GroupMemberResponse = GroupMemberResponse {
  groupMemberResponseId :: !(Int64),
  groupMemberResponseUserId :: !(Int64),
  groupMemberResponseGlobalGroupId :: !(Int64),
  groupMemberResponseCreatedAt :: !((Maybe UTCTime)),
  groupMemberResponseModifiedBy :: !((Maybe Int64)),
  groupMemberResponseModifiedAt :: !((Maybe UTCTime)),
  groupMemberResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON GroupMemberResponse where
  parseJSON (Object o) = do
    groupMemberResponseId <- o .: ("id" :: Text)
    groupMemberResponseUserId <- o .: ("user_id" :: Text)
    groupMemberResponseGlobalGroupId <- o .: ("global_group_id" :: Text)
    groupMemberResponseCreatedAt <- o .: ("created_at" :: Text)
    groupMemberResponseModifiedBy <- o .: ("modified_by" :: Text)
    groupMemberResponseModifiedAt <- o .: ("modified_at" :: Text)
    groupMemberResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ GroupMemberResponse {
      groupMemberResponseId = groupMemberResponseId,
      groupMemberResponseUserId = groupMemberResponseUserId,
      groupMemberResponseGlobalGroupId = groupMemberResponseGlobalGroupId,
      groupMemberResponseCreatedAt = groupMemberResponseCreatedAt,
      groupMemberResponseModifiedBy = groupMemberResponseModifiedBy,
      groupMemberResponseModifiedAt = groupMemberResponseModifiedAt,
      groupMemberResponseActivityAt = groupMemberResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GroupMemberResponse where
  toJSON GroupMemberResponse{..} = object $
    [ "tag" .= ("GroupMemberResponse" :: Text)
    , "id" .= groupMemberResponseId
    , "user_id" .= groupMemberResponseUserId
    , "global_group_id" .= groupMemberResponseGlobalGroupId
    , "created_at" .= groupMemberResponseCreatedAt
    , "modified_by" .= groupMemberResponseModifiedBy
    , "modified_at" .= groupMemberResponseModifiedAt
    , "activity_at" .= groupMemberResponseActivityAt
    ]


instance Eq GroupMemberResponse where
  (==) a b = groupMemberResponseId a == groupMemberResponseId b && groupMemberResponseUserId a == groupMemberResponseUserId b && groupMemberResponseGlobalGroupId a == groupMemberResponseGlobalGroupId b && groupMemberResponseCreatedAt a == groupMemberResponseCreatedAt b && groupMemberResponseModifiedBy a == groupMemberResponseModifiedBy b && groupMemberResponseModifiedAt a == groupMemberResponseModifiedAt b && groupMemberResponseActivityAt a == groupMemberResponseActivityAt b

instance Show GroupMemberResponse where
    show rec = "groupMemberResponseId: " <> show (groupMemberResponseId rec) <> ", " <> "groupMemberResponseUserId: " <> show (groupMemberResponseUserId rec) <> ", " <> "groupMemberResponseGlobalGroupId: " <> show (groupMemberResponseGlobalGroupId rec) <> ", " <> "groupMemberResponseCreatedAt: " <> show (groupMemberResponseCreatedAt rec) <> ", " <> "groupMemberResponseModifiedBy: " <> show (groupMemberResponseModifiedBy rec) <> ", " <> "groupMemberResponseModifiedAt: " <> show (groupMemberResponseModifiedAt rec) <> ", " <> "groupMemberResponseActivityAt: " <> show (groupMemberResponseActivityAt rec)

data GroupMemberResponses = GroupMemberResponses {
  groupMemberResponses :: !([GroupMemberResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON GroupMemberResponses where
  parseJSON (Object o) = do
    groupMemberResponses <- o .: ("group_member_responses" :: Text)
    pure $ GroupMemberResponses {
      groupMemberResponses = groupMemberResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GroupMemberResponses where
  toJSON GroupMemberResponses{..} = object $
    [ "tag" .= ("GroupMemberResponses" :: Text)
    , "group_member_responses" .= groupMemberResponses
    ]


instance Eq GroupMemberResponses where
  (==) a b = groupMemberResponses a == groupMemberResponses b

instance Show GroupMemberResponses where
    show rec = "groupMemberResponses: " <> show (groupMemberResponses rec)

data GroupMemberStatResponse
  = GroupMemberStatResponse 
  deriving (Generic,Typeable,NFData)


instance FromJSON GroupMemberStatResponse where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("GroupMemberStatResponse" :: Text) -> do
        pure GroupMemberStatResponse

      _ -> fail "Could not parse GroupMemberStatResponse"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GroupMemberStatResponse where
  toJSON (GroupMemberStatResponse ) = object $
    [ "tag" .= ("GroupMemberStatResponse" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq GroupMemberStatResponse where
  (==) GroupMemberStatResponse GroupMemberStatResponse = True


instance Show GroupMemberStatResponse where
  show GroupMemberStatResponse = "group_member_stat_response"


data GroupMemberStatResponses
  = GroupMemberStatResponses 
  deriving (Generic,Typeable,NFData)


instance FromJSON GroupMemberStatResponses where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("GroupMemberStatResponses" :: Text) -> do
        pure GroupMemberStatResponses

      _ -> fail "Could not parse GroupMemberStatResponses"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GroupMemberStatResponses where
  toJSON (GroupMemberStatResponses ) = object $
    [ "tag" .= ("GroupMemberStatResponses" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq GroupMemberStatResponses where
  (==) GroupMemberStatResponses GroupMemberStatResponses = True


instance Show GroupMemberStatResponses where
  show GroupMemberStatResponses = "group_member_stat_responses"

-- footer