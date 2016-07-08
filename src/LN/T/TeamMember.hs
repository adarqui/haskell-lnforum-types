{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.TeamMember where




import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)

instance FromJSON TeamMemberRequest where
  parseJSON (Object o) = do
    teamMemberRequestGuard <- o .: ("guard" :: Text)
    pure $ TeamMemberRequest {
      teamMemberRequestGuard = teamMemberRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TeamMemberRequest where
  toJSON TeamMemberRequest{..} = object $
    [ "tag" .= ("TeamMemberRequest" :: Text)
    , "guard" .= teamMemberRequestGuard
    ]


instance Eq TeamMemberRequest where
  (==) a b = teamMemberRequestGuard a == teamMemberRequestGuard b

instance Show TeamMemberRequest where
    show rec = "teamMemberRequestGuard: " <> show (teamMemberRequestGuard rec)

instance FromJSON TeamMemberResponse where
  parseJSON (Object o) = do
    teamMemberResponseId <- o .: ("id" :: Text)
    teamMemberResponseUserId <- o .: ("user_id" :: Text)
    teamMemberResponseOrgId <- o .: ("org_id" :: Text)
    teamMemberResponseTeamId <- o .: ("team_id" :: Text)
    teamMemberResponseIsAccepted <- o .: ("is_accepted" :: Text)
    teamMemberResponseAcceptedAt <- o .: ("accepted_at" :: Text)
    teamMemberResponseIsBlocked <- o .: ("is_blocked" :: Text)
    teamMemberResponseBlockedAt <- o .: ("blocked_at" :: Text)
    teamMemberResponseActive <- o .: ("active" :: Text)
    teamMemberResponseGuard <- o .: ("guard" :: Text)
    teamMemberResponseCreatedAt <- o .: ("created_at" :: Text)
    teamMemberResponseModifiedBy <- o .: ("modified_by" :: Text)
    teamMemberResponseModifiedAt <- o .: ("modified_at" :: Text)
    teamMemberResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ TeamMemberResponse {
      teamMemberResponseId = teamMemberResponseId,
      teamMemberResponseUserId = teamMemberResponseUserId,
      teamMemberResponseOrgId = teamMemberResponseOrgId,
      teamMemberResponseTeamId = teamMemberResponseTeamId,
      teamMemberResponseIsAccepted = teamMemberResponseIsAccepted,
      teamMemberResponseAcceptedAt = teamMemberResponseAcceptedAt,
      teamMemberResponseIsBlocked = teamMemberResponseIsBlocked,
      teamMemberResponseBlockedAt = teamMemberResponseBlockedAt,
      teamMemberResponseActive = teamMemberResponseActive,
      teamMemberResponseGuard = teamMemberResponseGuard,
      teamMemberResponseCreatedAt = teamMemberResponseCreatedAt,
      teamMemberResponseModifiedBy = teamMemberResponseModifiedBy,
      teamMemberResponseModifiedAt = teamMemberResponseModifiedAt,
      teamMemberResponseActivityAt = teamMemberResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TeamMemberResponse where
  toJSON TeamMemberResponse{..} = object $
    [ "tag" .= ("TeamMemberResponse" :: Text)
    , "id" .= teamMemberResponseId
    , "user_id" .= teamMemberResponseUserId
    , "org_id" .= teamMemberResponseOrgId
    , "team_id" .= teamMemberResponseTeamId
    , "is_accepted" .= teamMemberResponseIsAccepted
    , "accepted_at" .= teamMemberResponseAcceptedAt
    , "is_blocked" .= teamMemberResponseIsBlocked
    , "blocked_at" .= teamMemberResponseBlockedAt
    , "active" .= teamMemberResponseActive
    , "guard" .= teamMemberResponseGuard
    , "created_at" .= teamMemberResponseCreatedAt
    , "modified_by" .= teamMemberResponseModifiedBy
    , "modified_at" .= teamMemberResponseModifiedAt
    , "activity_at" .= teamMemberResponseActivityAt
    ]


instance Eq TeamMemberResponse where
  (==) a b = teamMemberResponseId a == teamMemberResponseId b && teamMemberResponseUserId a == teamMemberResponseUserId b && teamMemberResponseOrgId a == teamMemberResponseOrgId b && teamMemberResponseTeamId a == teamMemberResponseTeamId b && teamMemberResponseIsAccepted a == teamMemberResponseIsAccepted b && teamMemberResponseAcceptedAt a == teamMemberResponseAcceptedAt b && teamMemberResponseIsBlocked a == teamMemberResponseIsBlocked b && teamMemberResponseBlockedAt a == teamMemberResponseBlockedAt b && teamMemberResponseActive a == teamMemberResponseActive b && teamMemberResponseGuard a == teamMemberResponseGuard b && teamMemberResponseCreatedAt a == teamMemberResponseCreatedAt b && teamMemberResponseModifiedBy a == teamMemberResponseModifiedBy b && teamMemberResponseModifiedAt a == teamMemberResponseModifiedAt b && teamMemberResponseActivityAt a == teamMemberResponseActivityAt b

instance Show TeamMemberResponse where
    show rec = "teamMemberResponseId: " <> show (teamMemberResponseId rec) <> ", " <> "teamMemberResponseUserId: " <> show (teamMemberResponseUserId rec) <> ", " <> "teamMemberResponseOrgId: " <> show (teamMemberResponseOrgId rec) <> ", " <> "teamMemberResponseTeamId: " <> show (teamMemberResponseTeamId rec) <> ", " <> "teamMemberResponseIsAccepted: " <> show (teamMemberResponseIsAccepted rec) <> ", " <> "teamMemberResponseAcceptedAt: " <> show (teamMemberResponseAcceptedAt rec) <> ", " <> "teamMemberResponseIsBlocked: " <> show (teamMemberResponseIsBlocked rec) <> ", " <> "teamMemberResponseBlockedAt: " <> show (teamMemberResponseBlockedAt rec) <> ", " <> "teamMemberResponseActive: " <> show (teamMemberResponseActive rec) <> ", " <> "teamMemberResponseGuard: " <> show (teamMemberResponseGuard rec) <> ", " <> "teamMemberResponseCreatedAt: " <> show (teamMemberResponseCreatedAt rec) <> ", " <> "teamMemberResponseModifiedBy: " <> show (teamMemberResponseModifiedBy rec) <> ", " <> "teamMemberResponseModifiedAt: " <> show (teamMemberResponseModifiedAt rec) <> ", " <> "teamMemberResponseActivityAt: " <> show (teamMemberResponseActivityAt rec)

instance FromJSON TeamMemberResponses where
  parseJSON (Object o) = do
    teamMemberResponses <- o .: ("team_member_responses" :: Text)
    pure $ TeamMemberResponses {
      teamMemberResponses = teamMemberResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TeamMemberResponses where
  toJSON TeamMemberResponses{..} = object $
    [ "tag" .= ("TeamMemberResponses" :: Text)
    , "team_member_responses" .= teamMemberResponses
    ]


instance Eq TeamMemberResponses where
  (==) a b = teamMemberResponses a == teamMemberResponses b

instance Show TeamMemberResponses where
    show rec = "teamMemberResponses: " <> show (teamMemberResponses rec)

instance FromJSON TeamMemberStatResponse where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("TeamMemberStatResponse" :: Text) -> do
        pure TeamMemberStatResponse

      _ -> fail "Could not parse TeamMemberStatResponse"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TeamMemberStatResponse where
  toJSON (TeamMemberStatResponse ) = object $
    [ "tag" .= ("TeamMemberStatResponse" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq TeamMemberStatResponse where
  (==) TeamMemberStatResponse TeamMemberStatResponse = True


instance Show TeamMemberStatResponse where
  show TeamMemberStatResponse = "team_member_stat_response"


instance FromJSON TeamMemberStatResponses where
  parseJSON (Object o) = do
    teamMemberStatResponses <- o .: ("team_member_stat_responses" :: Text)
    pure $ TeamMemberStatResponses {
      teamMemberStatResponses = teamMemberStatResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TeamMemberStatResponses where
  toJSON TeamMemberStatResponses{..} = object $
    [ "tag" .= ("TeamMemberStatResponses" :: Text)
    , "team_member_stat_responses" .= teamMemberStatResponses
    ]


instance Eq TeamMemberStatResponses where
  (==) a b = teamMemberStatResponses a == teamMemberStatResponses b

instance Show TeamMemberStatResponses where
    show rec = "teamMemberStatResponses: " <> show (teamMemberStatResponses rec)
-- footer