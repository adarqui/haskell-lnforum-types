{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Organization where


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
import           Prelude

data OrganizationRequest = OrganizationRequest {
  organizationRequestDisplayName :: !(Text),
  organizationRequestDescription :: !((Maybe Text)),
  organizationRequestCompany :: !(Text),
  organizationRequestLocation :: !(Text),
  organizationRequestEmail :: !(Text),
  organizationRequestMembership :: !(Membership),
  organizationRequestTags :: !([Text]),
  organizationRequestIcon :: !((Maybe Text)),
  organizationRequestVisibility :: !(Visibility),
  organizationRequestGuard :: !(Int),
  organizationRequestStateTag :: !((Maybe Text))
}  deriving (Generic,Typeable,NFData)


instance FromJSON OrganizationRequest where
  parseJSON (Object o) = do
    organizationRequestDisplayName <- o .: ("display_name" :: Text)
    organizationRequestDescription <- o .: ("description" :: Text)
    organizationRequestCompany <- o .: ("company" :: Text)
    organizationRequestLocation <- o .: ("location" :: Text)
    organizationRequestEmail <- o .: ("email" :: Text)
    organizationRequestMembership <- o .: ("membership" :: Text)
    organizationRequestTags <- o .: ("tags" :: Text)
    organizationRequestIcon <- o .: ("icon" :: Text)
    organizationRequestVisibility <- o .: ("visibility" :: Text)
    organizationRequestGuard <- o .: ("guard" :: Text)
    organizationRequestStateTag <- o .: ("state_tag" :: Text)
    pure $ OrganizationRequest {
      organizationRequestDisplayName = organizationRequestDisplayName,
      organizationRequestDescription = organizationRequestDescription,
      organizationRequestCompany = organizationRequestCompany,
      organizationRequestLocation = organizationRequestLocation,
      organizationRequestEmail = organizationRequestEmail,
      organizationRequestMembership = organizationRequestMembership,
      organizationRequestTags = organizationRequestTags,
      organizationRequestIcon = organizationRequestIcon,
      organizationRequestVisibility = organizationRequestVisibility,
      organizationRequestGuard = organizationRequestGuard,
      organizationRequestStateTag = organizationRequestStateTag
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON OrganizationRequest where
  toJSON OrganizationRequest{..} = object $
    [ "tag" .= ("OrganizationRequest" :: Text)
    , "display_name" .= organizationRequestDisplayName
    , "description" .= organizationRequestDescription
    , "company" .= organizationRequestCompany
    , "location" .= organizationRequestLocation
    , "email" .= organizationRequestEmail
    , "membership" .= organizationRequestMembership
    , "tags" .= organizationRequestTags
    , "icon" .= organizationRequestIcon
    , "visibility" .= organizationRequestVisibility
    , "guard" .= organizationRequestGuard
    , "state_tag" .= organizationRequestStateTag
    ]


instance Eq OrganizationRequest where
  (==) a b = organizationRequestDisplayName a == organizationRequestDisplayName b && organizationRequestDescription a == organizationRequestDescription b && organizationRequestCompany a == organizationRequestCompany b && organizationRequestLocation a == organizationRequestLocation b && organizationRequestEmail a == organizationRequestEmail b && organizationRequestMembership a == organizationRequestMembership b && organizationRequestTags a == organizationRequestTags b && organizationRequestIcon a == organizationRequestIcon b && organizationRequestVisibility a == organizationRequestVisibility b && organizationRequestGuard a == organizationRequestGuard b && organizationRequestStateTag a == organizationRequestStateTag b

instance Show OrganizationRequest where
    show rec = "organizationRequestDisplayName: " <> show (organizationRequestDisplayName rec) <> ", " <> "organizationRequestDescription: " <> show (organizationRequestDescription rec) <> ", " <> "organizationRequestCompany: " <> show (organizationRequestCompany rec) <> ", " <> "organizationRequestLocation: " <> show (organizationRequestLocation rec) <> ", " <> "organizationRequestEmail: " <> show (organizationRequestEmail rec) <> ", " <> "organizationRequestMembership: " <> show (organizationRequestMembership rec) <> ", " <> "organizationRequestTags: " <> show (organizationRequestTags rec) <> ", " <> "organizationRequestIcon: " <> show (organizationRequestIcon rec) <> ", " <> "organizationRequestVisibility: " <> show (organizationRequestVisibility rec) <> ", " <> "organizationRequestGuard: " <> show (organizationRequestGuard rec) <> ", " <> "organizationRequestStateTag: " <> show (organizationRequestStateTag rec)

data OrganizationResponse = OrganizationResponse {
  organizationResponseId :: !(Int64),
  organizationResponseUserId :: !(Int64),
  organizationResponseName :: !(Text),
  organizationResponseDisplayName :: !(Text),
  organizationResponseDescription :: !((Maybe Text)),
  organizationResponseCompany :: !(Text),
  organizationResponseLocation :: !(Text),
  organizationResponseEmail :: !(Text),
  organizationResponseEmailMD5 :: !(Text),
  organizationResponseMembership :: !(Membership),
  organizationResponseIcon :: !((Maybe Text)),
  organizationResponseTags :: !([Text]),
  organizationResponseVisibility :: !(Visibility),
  organizationResponseActive :: !(Bool),
  organizationResponseGuard :: !(Int),
  organizationResponseCreatedAt :: !((Maybe UTCTime)),
  organizationResponseModifiedBy :: !((Maybe Int64)),
  organizationResponseModifiedAt :: !((Maybe UTCTime)),
  organizationResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON OrganizationResponse where
  parseJSON (Object o) = do
    organizationResponseId <- o .: ("id" :: Text)
    organizationResponseUserId <- o .: ("user_id" :: Text)
    organizationResponseName <- o .: ("name" :: Text)
    organizationResponseDisplayName <- o .: ("display_name" :: Text)
    organizationResponseDescription <- o .: ("description" :: Text)
    organizationResponseCompany <- o .: ("company" :: Text)
    organizationResponseLocation <- o .: ("location" :: Text)
    organizationResponseEmail <- o .: ("email" :: Text)
    organizationResponseEmailMD5 <- o .: ("email_md5" :: Text)
    organizationResponseMembership <- o .: ("membership" :: Text)
    organizationResponseIcon <- o .: ("icon" :: Text)
    organizationResponseTags <- o .: ("tags" :: Text)
    organizationResponseVisibility <- o .: ("visibility" :: Text)
    organizationResponseActive <- o .: ("active" :: Text)
    organizationResponseGuard <- o .: ("guard" :: Text)
    organizationResponseCreatedAt <- o .: ("created_at" :: Text)
    organizationResponseModifiedBy <- o .: ("modified_by" :: Text)
    organizationResponseModifiedAt <- o .: ("modified_at" :: Text)
    organizationResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ OrganizationResponse {
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
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON OrganizationResponse where
  toJSON OrganizationResponse{..} = object $
    [ "tag" .= ("OrganizationResponse" :: Text)
    , "id" .= organizationResponseId
    , "user_id" .= organizationResponseUserId
    , "name" .= organizationResponseName
    , "display_name" .= organizationResponseDisplayName
    , "description" .= organizationResponseDescription
    , "company" .= organizationResponseCompany
    , "location" .= organizationResponseLocation
    , "email" .= organizationResponseEmail
    , "email_md5" .= organizationResponseEmailMD5
    , "membership" .= organizationResponseMembership
    , "icon" .= organizationResponseIcon
    , "tags" .= organizationResponseTags
    , "visibility" .= organizationResponseVisibility
    , "active" .= organizationResponseActive
    , "guard" .= organizationResponseGuard
    , "created_at" .= organizationResponseCreatedAt
    , "modified_by" .= organizationResponseModifiedBy
    , "modified_at" .= organizationResponseModifiedAt
    , "activity_at" .= organizationResponseActivityAt
    ]


instance Eq OrganizationResponse where
  (==) a b = organizationResponseId a == organizationResponseId b && organizationResponseUserId a == organizationResponseUserId b && organizationResponseName a == organizationResponseName b && organizationResponseDisplayName a == organizationResponseDisplayName b && organizationResponseDescription a == organizationResponseDescription b && organizationResponseCompany a == organizationResponseCompany b && organizationResponseLocation a == organizationResponseLocation b && organizationResponseEmail a == organizationResponseEmail b && organizationResponseEmailMD5 a == organizationResponseEmailMD5 b && organizationResponseMembership a == organizationResponseMembership b && organizationResponseIcon a == organizationResponseIcon b && organizationResponseTags a == organizationResponseTags b && organizationResponseVisibility a == organizationResponseVisibility b && organizationResponseActive a == organizationResponseActive b && organizationResponseGuard a == organizationResponseGuard b && organizationResponseCreatedAt a == organizationResponseCreatedAt b && organizationResponseModifiedBy a == organizationResponseModifiedBy b && organizationResponseModifiedAt a == organizationResponseModifiedAt b && organizationResponseActivityAt a == organizationResponseActivityAt b

instance Show OrganizationResponse where
    show rec = "organizationResponseId: " <> show (organizationResponseId rec) <> ", " <> "organizationResponseUserId: " <> show (organizationResponseUserId rec) <> ", " <> "organizationResponseName: " <> show (organizationResponseName rec) <> ", " <> "organizationResponseDisplayName: " <> show (organizationResponseDisplayName rec) <> ", " <> "organizationResponseDescription: " <> show (organizationResponseDescription rec) <> ", " <> "organizationResponseCompany: " <> show (organizationResponseCompany rec) <> ", " <> "organizationResponseLocation: " <> show (organizationResponseLocation rec) <> ", " <> "organizationResponseEmail: " <> show (organizationResponseEmail rec) <> ", " <> "organizationResponseEmailMD5: " <> show (organizationResponseEmailMD5 rec) <> ", " <> "organizationResponseMembership: " <> show (organizationResponseMembership rec) <> ", " <> "organizationResponseIcon: " <> show (organizationResponseIcon rec) <> ", " <> "organizationResponseTags: " <> show (organizationResponseTags rec) <> ", " <> "organizationResponseVisibility: " <> show (organizationResponseVisibility rec) <> ", " <> "organizationResponseActive: " <> show (organizationResponseActive rec) <> ", " <> "organizationResponseGuard: " <> show (organizationResponseGuard rec) <> ", " <> "organizationResponseCreatedAt: " <> show (organizationResponseCreatedAt rec) <> ", " <> "organizationResponseModifiedBy: " <> show (organizationResponseModifiedBy rec) <> ", " <> "organizationResponseModifiedAt: " <> show (organizationResponseModifiedAt rec) <> ", " <> "organizationResponseActivityAt: " <> show (organizationResponseActivityAt rec)

data OrganizationResponses = OrganizationResponses {
  organizationResponses :: !([OrganizationResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON OrganizationResponses where
  parseJSON (Object o) = do
    organizationResponses <- o .: ("organization_responses" :: Text)
    pure $ OrganizationResponses {
      organizationResponses = organizationResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON OrganizationResponses where
  toJSON OrganizationResponses{..} = object $
    [ "tag" .= ("OrganizationResponses" :: Text)
    , "organization_responses" .= organizationResponses
    ]


instance Eq OrganizationResponses where
  (==) a b = organizationResponses a == organizationResponses b

instance Show OrganizationResponses where
    show rec = "organizationResponses: " <> show (organizationResponses rec)

data OrganizationStatResponse = OrganizationStatResponse {
  organizationStatResponseOrganizationId :: !(Int64),
  organizationStatResponseTeams :: !(Int64),
  organizationStatResponseMembers :: !(Int64),
  organizationStatResponseForums :: !(Int64),
  organizationStatResponseBoards :: !(Int64),
  organizationStatResponseThreads :: !(Int64),
  organizationStatResponseThreadPosts :: !(Int64),
  organizationStatResponseViews :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON OrganizationStatResponse where
  parseJSON (Object o) = do
    organizationStatResponseOrganizationId <- o .: ("organization_id" :: Text)
    organizationStatResponseTeams <- o .: ("teams" :: Text)
    organizationStatResponseMembers <- o .: ("members" :: Text)
    organizationStatResponseForums <- o .: ("forums" :: Text)
    organizationStatResponseBoards <- o .: ("boards" :: Text)
    organizationStatResponseThreads <- o .: ("threads" :: Text)
    organizationStatResponseThreadPosts <- o .: ("thread_posts" :: Text)
    organizationStatResponseViews <- o .: ("views" :: Text)
    pure $ OrganizationStatResponse {
      organizationStatResponseOrganizationId = organizationStatResponseOrganizationId,
      organizationStatResponseTeams = organizationStatResponseTeams,
      organizationStatResponseMembers = organizationStatResponseMembers,
      organizationStatResponseForums = organizationStatResponseForums,
      organizationStatResponseBoards = organizationStatResponseBoards,
      organizationStatResponseThreads = organizationStatResponseThreads,
      organizationStatResponseThreadPosts = organizationStatResponseThreadPosts,
      organizationStatResponseViews = organizationStatResponseViews
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON OrganizationStatResponse where
  toJSON OrganizationStatResponse{..} = object $
    [ "tag" .= ("OrganizationStatResponse" :: Text)
    , "organization_id" .= organizationStatResponseOrganizationId
    , "teams" .= organizationStatResponseTeams
    , "members" .= organizationStatResponseMembers
    , "forums" .= organizationStatResponseForums
    , "boards" .= organizationStatResponseBoards
    , "threads" .= organizationStatResponseThreads
    , "thread_posts" .= organizationStatResponseThreadPosts
    , "views" .= organizationStatResponseViews
    ]


instance Eq OrganizationStatResponse where
  (==) a b = organizationStatResponseOrganizationId a == organizationStatResponseOrganizationId b && organizationStatResponseTeams a == organizationStatResponseTeams b && organizationStatResponseMembers a == organizationStatResponseMembers b && organizationStatResponseForums a == organizationStatResponseForums b && organizationStatResponseBoards a == organizationStatResponseBoards b && organizationStatResponseThreads a == organizationStatResponseThreads b && organizationStatResponseThreadPosts a == organizationStatResponseThreadPosts b && organizationStatResponseViews a == organizationStatResponseViews b

instance Show OrganizationStatResponse where
    show rec = "organizationStatResponseOrganizationId: " <> show (organizationStatResponseOrganizationId rec) <> ", " <> "organizationStatResponseTeams: " <> show (organizationStatResponseTeams rec) <> ", " <> "organizationStatResponseMembers: " <> show (organizationStatResponseMembers rec) <> ", " <> "organizationStatResponseForums: " <> show (organizationStatResponseForums rec) <> ", " <> "organizationStatResponseBoards: " <> show (organizationStatResponseBoards rec) <> ", " <> "organizationStatResponseThreads: " <> show (organizationStatResponseThreads rec) <> ", " <> "organizationStatResponseThreadPosts: " <> show (organizationStatResponseThreadPosts rec) <> ", " <> "organizationStatResponseViews: " <> show (organizationStatResponseViews rec)

data OrganizationStatResponses = OrganizationStatResponses {
  organizationStatResponses :: !([OrganizationStatResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON OrganizationStatResponses where
  parseJSON (Object o) = do
    organizationStatResponses <- o .: ("organization_stat_responses" :: Text)
    pure $ OrganizationStatResponses {
      organizationStatResponses = organizationStatResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON OrganizationStatResponses where
  toJSON OrganizationStatResponses{..} = object $
    [ "tag" .= ("OrganizationStatResponses" :: Text)
    , "organization_stat_responses" .= organizationStatResponses
    ]


instance Eq OrganizationStatResponses where
  (==) a b = organizationStatResponses a == organizationStatResponses b

instance Show OrganizationStatResponses where
    show rec = "organizationStatResponses: " <> show (organizationStatResponses rec)
-- footer