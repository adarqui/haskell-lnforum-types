{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.Organization where


import LN.T.Organization
import LN.T.User
import LN.T.Team
import LN.T.Like
import LN.T.Star
import LN.T.Permission


import           Control.DeepSeq     (NFData)
import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Default
import           Data.Int            (Int64)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Typeable       (Typeable)
import           Data.Monoid         ((<>))
import           GHC.Generics        (Generic)
import           Haskell.Api.Helpers (QueryParam, qp)

data OrganizationPackResponse = OrganizationPackResponse {
  organizationPackResponseUser :: !(UserSanitizedResponse),
  organizationPackResponseUserId :: !(Int64),
  organizationPackResponseOrganization :: !(OrganizationResponse),
  organizationPackResponseOrganizationId :: !(Int64),
  organizationPackResponseStat :: !(OrganizationStatResponse),
  organizationPackResponseLike :: !((Maybe LikeResponse)),
  organizationPackResponseStar :: !((Maybe StarResponse)),
  organizationPackResponsePermissions :: !(Permissions),
  organizationPackResponseTeams :: !([SystemTeam])
}  deriving (Generic,Typeable,NFData)


instance FromJSON OrganizationPackResponse where
  parseJSON (Object o) = do
    organizationPackResponseUser <- o .: ("user" :: Text)
    organizationPackResponseUserId <- o .: ("user_id" :: Text)
    organizationPackResponseOrganization <- o .: ("organization" :: Text)
    organizationPackResponseOrganizationId <- o .: ("organization_id" :: Text)
    organizationPackResponseStat <- o .: ("stat" :: Text)
    organizationPackResponseLike <- o .: ("like" :: Text)
    organizationPackResponseStar <- o .: ("star" :: Text)
    organizationPackResponsePermissions <- o .: ("permissions" :: Text)
    organizationPackResponseTeams <- o .: ("teams" :: Text)
    pure $ OrganizationPackResponse {
      organizationPackResponseUser = organizationPackResponseUser,
      organizationPackResponseUserId = organizationPackResponseUserId,
      organizationPackResponseOrganization = organizationPackResponseOrganization,
      organizationPackResponseOrganizationId = organizationPackResponseOrganizationId,
      organizationPackResponseStat = organizationPackResponseStat,
      organizationPackResponseLike = organizationPackResponseLike,
      organizationPackResponseStar = organizationPackResponseStar,
      organizationPackResponsePermissions = organizationPackResponsePermissions,
      organizationPackResponseTeams = organizationPackResponseTeams
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON OrganizationPackResponse where
  toJSON OrganizationPackResponse{..} = object $
    [ "tag" .= ("OrganizationPackResponse" :: Text)
    , "user" .= organizationPackResponseUser
    , "user_id" .= organizationPackResponseUserId
    , "organization" .= organizationPackResponseOrganization
    , "organization_id" .= organizationPackResponseOrganizationId
    , "stat" .= organizationPackResponseStat
    , "like" .= organizationPackResponseLike
    , "star" .= organizationPackResponseStar
    , "permissions" .= organizationPackResponsePermissions
    , "teams" .= organizationPackResponseTeams
    ]


instance Eq OrganizationPackResponse where
  (==) a b = organizationPackResponseUser a == organizationPackResponseUser b && organizationPackResponseUserId a == organizationPackResponseUserId b && organizationPackResponseOrganization a == organizationPackResponseOrganization b && organizationPackResponseOrganizationId a == organizationPackResponseOrganizationId b && organizationPackResponseStat a == organizationPackResponseStat b && organizationPackResponseLike a == organizationPackResponseLike b && organizationPackResponseStar a == organizationPackResponseStar b && organizationPackResponsePermissions a == organizationPackResponsePermissions b && organizationPackResponseTeams a == organizationPackResponseTeams b

instance Show OrganizationPackResponse where
    show rec = "organizationPackResponseUser: " <> show (organizationPackResponseUser rec) <> ", " <> "organizationPackResponseUserId: " <> show (organizationPackResponseUserId rec) <> ", " <> "organizationPackResponseOrganization: " <> show (organizationPackResponseOrganization rec) <> ", " <> "organizationPackResponseOrganizationId: " <> show (organizationPackResponseOrganizationId rec) <> ", " <> "organizationPackResponseStat: " <> show (organizationPackResponseStat rec) <> ", " <> "organizationPackResponseLike: " <> show (organizationPackResponseLike rec) <> ", " <> "organizationPackResponseStar: " <> show (organizationPackResponseStar rec) <> ", " <> "organizationPackResponsePermissions: " <> show (organizationPackResponsePermissions rec) <> ", " <> "organizationPackResponseTeams: " <> show (organizationPackResponseTeams rec)

data OrganizationPackResponses = OrganizationPackResponses {
  organizationPackResponses :: !([OrganizationPackResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON OrganizationPackResponses where
  parseJSON (Object o) = do
    organizationPackResponses <- o .: ("organization_pack_responses" :: Text)
    pure $ OrganizationPackResponses {
      organizationPackResponses = organizationPackResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON OrganizationPackResponses where
  toJSON OrganizationPackResponses{..} = object $
    [ "tag" .= ("OrganizationPackResponses" :: Text)
    , "organization_pack_responses" .= organizationPackResponses
    ]


instance Eq OrganizationPackResponses where
  (==) a b = organizationPackResponses a == organizationPackResponses b

instance Show OrganizationPackResponses where
    show rec = "organizationPackResponses: " <> show (organizationPackResponses rec)
-- footer