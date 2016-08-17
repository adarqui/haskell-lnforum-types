{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.Group where


import LN.T.Group
import LN.T.User
import LN.T.Permission
import LN.T.Organization


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

data GroupPackResponse = GroupPackResponse {
  groupPackResponseUser :: !(UserSanitizedResponse),
  groupPackResponseUserId :: !(Int64),
  groupPackResponseGroup :: !(GroupResponse),
  groupPackResponseGroupId :: !(Int64),
  groupPackResponseOrganization :: !(OrganizationResponse),
  groupPackResponseOrganizationId :: !(Int64),
  groupPackResponseStat :: !(GroupStatResponse),
  groupPackResponsePermissions :: !(Permissions)
}  deriving (Generic,Typeable,NFData)


instance FromJSON GroupPackResponse where
  parseJSON (Object o) = do
    groupPackResponseUser <- o .: ("user" :: Text)
    groupPackResponseUserId <- o .: ("user_id" :: Text)
    groupPackResponseGroup <- o .: ("group" :: Text)
    groupPackResponseGroupId <- o .: ("group_id" :: Text)
    groupPackResponseOrganization <- o .: ("organization" :: Text)
    groupPackResponseOrganizationId <- o .: ("organization_id" :: Text)
    groupPackResponseStat <- o .: ("stat" :: Text)
    groupPackResponsePermissions <- o .: ("permissions" :: Text)
    pure $ GroupPackResponse {
      groupPackResponseUser = groupPackResponseUser,
      groupPackResponseUserId = groupPackResponseUserId,
      groupPackResponseGroup = groupPackResponseGroup,
      groupPackResponseGroupId = groupPackResponseGroupId,
      groupPackResponseOrganization = groupPackResponseOrganization,
      groupPackResponseOrganizationId = groupPackResponseOrganizationId,
      groupPackResponseStat = groupPackResponseStat,
      groupPackResponsePermissions = groupPackResponsePermissions
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GroupPackResponse where
  toJSON GroupPackResponse{..} = object $
    [ "tag" .= ("GroupPackResponse" :: Text)
    , "user" .= groupPackResponseUser
    , "user_id" .= groupPackResponseUserId
    , "group" .= groupPackResponseGroup
    , "group_id" .= groupPackResponseGroupId
    , "organization" .= groupPackResponseOrganization
    , "organization_id" .= groupPackResponseOrganizationId
    , "stat" .= groupPackResponseStat
    , "permissions" .= groupPackResponsePermissions
    ]


instance Eq GroupPackResponse where
  (==) a b = groupPackResponseUser a == groupPackResponseUser b && groupPackResponseUserId a == groupPackResponseUserId b && groupPackResponseGroup a == groupPackResponseGroup b && groupPackResponseGroupId a == groupPackResponseGroupId b && groupPackResponseOrganization a == groupPackResponseOrganization b && groupPackResponseOrganizationId a == groupPackResponseOrganizationId b && groupPackResponseStat a == groupPackResponseStat b && groupPackResponsePermissions a == groupPackResponsePermissions b

instance Show GroupPackResponse where
    show rec = "groupPackResponseUser: " <> show (groupPackResponseUser rec) <> ", " <> "groupPackResponseUserId: " <> show (groupPackResponseUserId rec) <> ", " <> "groupPackResponseGroup: " <> show (groupPackResponseGroup rec) <> ", " <> "groupPackResponseGroupId: " <> show (groupPackResponseGroupId rec) <> ", " <> "groupPackResponseOrganization: " <> show (groupPackResponseOrganization rec) <> ", " <> "groupPackResponseOrganizationId: " <> show (groupPackResponseOrganizationId rec) <> ", " <> "groupPackResponseStat: " <> show (groupPackResponseStat rec) <> ", " <> "groupPackResponsePermissions: " <> show (groupPackResponsePermissions rec)

data GroupPackResponses = GroupPackResponses {
  groupPackResponses :: !([GroupPackResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON GroupPackResponses where
  parseJSON (Object o) = do
    groupPackResponses <- o .: ("group_pack_responses" :: Text)
    pure $ GroupPackResponses {
      groupPackResponses = groupPackResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GroupPackResponses where
  toJSON GroupPackResponses{..} = object $
    [ "tag" .= ("GroupPackResponses" :: Text)
    , "group_pack_responses" .= groupPackResponses
    ]


instance Eq GroupPackResponses where
  (==) a b = groupPackResponses a == groupPackResponses b

instance Show GroupPackResponses where
    show rec = "groupPackResponses: " <> show (groupPackResponses rec)
-- footer