{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Team where


import LN.T.Visibility
import LN.T.Membership


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

data SystemTeam
  = Team_Owners 
  | Team_Members 
  deriving (Generic,Typeable,NFData)


instance FromJSON SystemTeam where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("Team_Owners" :: Text) -> do
        pure Team_Owners

      ("Team_Members" :: Text) -> do
        pure Team_Members

      _ -> fail "Could not parse SystemTeam"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON SystemTeam where
  toJSON (Team_Owners ) = object $
    [ "tag" .= ("Team_Owners" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Team_Members ) = object $
    [ "tag" .= ("Team_Members" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq SystemTeam where
  (==) Team_Owners Team_Owners = True
  (==) Team_Members Team_Members = True
  (==) _ _ = False

instance Show SystemTeam where
  show Team_Owners = "team_owners"
  show Team_Members = "team_members"


instance Read SystemTeam where
  readsPrec _ "team_owners" = [(Team_Owners, "")]
  readsPrec _ "team_members" = [(Team_Members, "")]
  readsPrec _ _ = []


data TeamRequest = TeamRequest {
  teamRequestMembership :: !(Membership),
  teamRequestIcon :: !((Maybe Text)),
  teamRequestTags :: !([Text]),
  teamRequestVisibility :: !(Visibility),
  teamRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON TeamRequest where
  parseJSON (Object o) = do
    teamRequestMembership <- o .: ("membership" :: Text)
    teamRequestIcon <- o .: ("icon" :: Text)
    teamRequestTags <- o .: ("tags" :: Text)
    teamRequestVisibility <- o .: ("visibility" :: Text)
    teamRequestGuard <- o .: ("guard" :: Text)
    pure $ TeamRequest {
      teamRequestMembership = teamRequestMembership,
      teamRequestIcon = teamRequestIcon,
      teamRequestTags = teamRequestTags,
      teamRequestVisibility = teamRequestVisibility,
      teamRequestGuard = teamRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TeamRequest where
  toJSON TeamRequest{..} = object $
    [ "tag" .= ("TeamRequest" :: Text)
    , "membership" .= teamRequestMembership
    , "icon" .= teamRequestIcon
    , "tags" .= teamRequestTags
    , "visibility" .= teamRequestVisibility
    , "guard" .= teamRequestGuard
    ]


instance Eq TeamRequest where
  (==) a b = teamRequestMembership a == teamRequestMembership b && teamRequestIcon a == teamRequestIcon b && teamRequestTags a == teamRequestTags b && teamRequestVisibility a == teamRequestVisibility b && teamRequestGuard a == teamRequestGuard b

instance Show TeamRequest where
    show rec = "teamRequestMembership: " <> show (teamRequestMembership rec) <> ", " <> "teamRequestIcon: " <> show (teamRequestIcon rec) <> ", " <> "teamRequestTags: " <> show (teamRequestTags rec) <> ", " <> "teamRequestVisibility: " <> show (teamRequestVisibility rec) <> ", " <> "teamRequestGuard: " <> show (teamRequestGuard rec)

data TeamResponse = TeamResponse {
  teamResponseId :: !(Int64),
  teamResponseUserId :: !(Int64),
  teamResponseOrgId :: !(Int64),
  teamResponseSystem :: !(SystemTeam),
  teamResponseMembership :: !(Membership),
  teamResponseIcon :: !((Maybe Text)),
  teamResponseTags :: !([Text]),
  teamResponseVisibility :: !(Visibility),
  teamResponseActive :: !(Bool),
  teamResponseGuard :: !(Int),
  teamResponseCreatedAt :: !((Maybe UTCTime)),
  teamResponseModifiedBy :: !((Maybe Int64)),
  teamResponseModifiedAt :: !((Maybe UTCTime)),
  teamResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON TeamResponse where
  parseJSON (Object o) = do
    teamResponseId <- o .: ("id" :: Text)
    teamResponseUserId <- o .: ("user_id" :: Text)
    teamResponseOrgId <- o .: ("org_id" :: Text)
    teamResponseSystem <- o .: ("system" :: Text)
    teamResponseMembership <- o .: ("membership" :: Text)
    teamResponseIcon <- o .: ("icon" :: Text)
    teamResponseTags <- o .: ("tags" :: Text)
    teamResponseVisibility <- o .: ("visibility" :: Text)
    teamResponseActive <- o .: ("active" :: Text)
    teamResponseGuard <- o .: ("guard" :: Text)
    teamResponseCreatedAt <- o .: ("created_at" :: Text)
    teamResponseModifiedBy <- o .: ("modified_by" :: Text)
    teamResponseModifiedAt <- o .: ("modified_at" :: Text)
    teamResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ TeamResponse {
      teamResponseId = teamResponseId,
      teamResponseUserId = teamResponseUserId,
      teamResponseOrgId = teamResponseOrgId,
      teamResponseSystem = teamResponseSystem,
      teamResponseMembership = teamResponseMembership,
      teamResponseIcon = teamResponseIcon,
      teamResponseTags = teamResponseTags,
      teamResponseVisibility = teamResponseVisibility,
      teamResponseActive = teamResponseActive,
      teamResponseGuard = teamResponseGuard,
      teamResponseCreatedAt = teamResponseCreatedAt,
      teamResponseModifiedBy = teamResponseModifiedBy,
      teamResponseModifiedAt = teamResponseModifiedAt,
      teamResponseActivityAt = teamResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TeamResponse where
  toJSON TeamResponse{..} = object $
    [ "tag" .= ("TeamResponse" :: Text)
    , "id" .= teamResponseId
    , "user_id" .= teamResponseUserId
    , "org_id" .= teamResponseOrgId
    , "system" .= teamResponseSystem
    , "membership" .= teamResponseMembership
    , "icon" .= teamResponseIcon
    , "tags" .= teamResponseTags
    , "visibility" .= teamResponseVisibility
    , "active" .= teamResponseActive
    , "guard" .= teamResponseGuard
    , "created_at" .= teamResponseCreatedAt
    , "modified_by" .= teamResponseModifiedBy
    , "modified_at" .= teamResponseModifiedAt
    , "activity_at" .= teamResponseActivityAt
    ]


instance Eq TeamResponse where
  (==) a b = teamResponseId a == teamResponseId b && teamResponseUserId a == teamResponseUserId b && teamResponseOrgId a == teamResponseOrgId b && teamResponseSystem a == teamResponseSystem b && teamResponseMembership a == teamResponseMembership b && teamResponseIcon a == teamResponseIcon b && teamResponseTags a == teamResponseTags b && teamResponseVisibility a == teamResponseVisibility b && teamResponseActive a == teamResponseActive b && teamResponseGuard a == teamResponseGuard b && teamResponseCreatedAt a == teamResponseCreatedAt b && teamResponseModifiedBy a == teamResponseModifiedBy b && teamResponseModifiedAt a == teamResponseModifiedAt b && teamResponseActivityAt a == teamResponseActivityAt b

instance Show TeamResponse where
    show rec = "teamResponseId: " <> show (teamResponseId rec) <> ", " <> "teamResponseUserId: " <> show (teamResponseUserId rec) <> ", " <> "teamResponseOrgId: " <> show (teamResponseOrgId rec) <> ", " <> "teamResponseSystem: " <> show (teamResponseSystem rec) <> ", " <> "teamResponseMembership: " <> show (teamResponseMembership rec) <> ", " <> "teamResponseIcon: " <> show (teamResponseIcon rec) <> ", " <> "teamResponseTags: " <> show (teamResponseTags rec) <> ", " <> "teamResponseVisibility: " <> show (teamResponseVisibility rec) <> ", " <> "teamResponseActive: " <> show (teamResponseActive rec) <> ", " <> "teamResponseGuard: " <> show (teamResponseGuard rec) <> ", " <> "teamResponseCreatedAt: " <> show (teamResponseCreatedAt rec) <> ", " <> "teamResponseModifiedBy: " <> show (teamResponseModifiedBy rec) <> ", " <> "teamResponseModifiedAt: " <> show (teamResponseModifiedAt rec) <> ", " <> "teamResponseActivityAt: " <> show (teamResponseActivityAt rec)

data TeamResponses = TeamResponses {
  teamResponses :: !([TeamResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON TeamResponses where
  parseJSON (Object o) = do
    teamResponses <- o .: ("team_responses" :: Text)
    pure $ TeamResponses {
      teamResponses = teamResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TeamResponses where
  toJSON TeamResponses{..} = object $
    [ "tag" .= ("TeamResponses" :: Text)
    , "team_responses" .= teamResponses
    ]


instance Eq TeamResponses where
  (==) a b = teamResponses a == teamResponses b

instance Show TeamResponses where
    show rec = "teamResponses: " <> show (teamResponses rec)

data TeamStatResponse = TeamStatResponse {
  teamStatResponseMembers :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON TeamStatResponse where
  parseJSON (Object o) = do
    teamStatResponseMembers <- o .: ("members" :: Text)
    pure $ TeamStatResponse {
      teamStatResponseMembers = teamStatResponseMembers
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TeamStatResponse where
  toJSON TeamStatResponse{..} = object $
    [ "tag" .= ("TeamStatResponse" :: Text)
    , "members" .= teamStatResponseMembers
    ]


instance Eq TeamStatResponse where
  (==) a b = teamStatResponseMembers a == teamStatResponseMembers b

instance Show TeamStatResponse where
    show rec = "teamStatResponseMembers: " <> show (teamStatResponseMembers rec)

data TeamStatResponses = TeamStatResponses {
  teamStatResponses :: !([TeamStatResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON TeamStatResponses where
  parseJSON (Object o) = do
    teamStatResponses <- o .: ("team_stat_responses" :: Text)
    pure $ TeamStatResponses {
      teamStatResponses = teamStatResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TeamStatResponses where
  toJSON TeamStatResponses{..} = object $
    [ "tag" .= ("TeamStatResponses" :: Text)
    , "team_stat_responses" .= teamStatResponses
    ]


instance Eq TeamStatResponses where
  (==) a b = teamStatResponses a == teamStatResponses b

instance Show TeamStatResponses where
    show rec = "teamStatResponses: " <> show (teamStatResponses rec)
-- footer