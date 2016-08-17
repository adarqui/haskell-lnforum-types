{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.Team where


import LN.T.Team
import LN.T.User
import LN.T.Permission


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

data TeamPackResponse = TeamPackResponse {
  teamPackResponseUser :: !(UserSanitizedResponse),
  teamPackResponseUserId :: !(Int64),
  teamPackResponseTeam :: !(TeamResponse),
  teamPackResponseTeamId :: !(Int64),
  teamPackResponseStat :: !(TeamStatResponse),
  teamPackResponsePermissions :: !(Permissions)
}  deriving (Generic,Typeable,NFData)


instance FromJSON TeamPackResponse where
  parseJSON (Object o) = do
    teamPackResponseUser <- o .: ("user" :: Text)
    teamPackResponseUserId <- o .: ("user_id" :: Text)
    teamPackResponseTeam <- o .: ("team" :: Text)
    teamPackResponseTeamId <- o .: ("team_id" :: Text)
    teamPackResponseStat <- o .: ("stat" :: Text)
    teamPackResponsePermissions <- o .: ("permissions" :: Text)
    pure $ TeamPackResponse {
      teamPackResponseUser = teamPackResponseUser,
      teamPackResponseUserId = teamPackResponseUserId,
      teamPackResponseTeam = teamPackResponseTeam,
      teamPackResponseTeamId = teamPackResponseTeamId,
      teamPackResponseStat = teamPackResponseStat,
      teamPackResponsePermissions = teamPackResponsePermissions
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TeamPackResponse where
  toJSON TeamPackResponse{..} = object $
    [ "tag" .= ("TeamPackResponse" :: Text)
    , "user" .= teamPackResponseUser
    , "user_id" .= teamPackResponseUserId
    , "team" .= teamPackResponseTeam
    , "team_id" .= teamPackResponseTeamId
    , "stat" .= teamPackResponseStat
    , "permissions" .= teamPackResponsePermissions
    ]


instance Eq TeamPackResponse where
  (==) a b = teamPackResponseUser a == teamPackResponseUser b && teamPackResponseUserId a == teamPackResponseUserId b && teamPackResponseTeam a == teamPackResponseTeam b && teamPackResponseTeamId a == teamPackResponseTeamId b && teamPackResponseStat a == teamPackResponseStat b && teamPackResponsePermissions a == teamPackResponsePermissions b

instance Show TeamPackResponse where
    show rec = "teamPackResponseUser: " <> show (teamPackResponseUser rec) <> ", " <> "teamPackResponseUserId: " <> show (teamPackResponseUserId rec) <> ", " <> "teamPackResponseTeam: " <> show (teamPackResponseTeam rec) <> ", " <> "teamPackResponseTeamId: " <> show (teamPackResponseTeamId rec) <> ", " <> "teamPackResponseStat: " <> show (teamPackResponseStat rec) <> ", " <> "teamPackResponsePermissions: " <> show (teamPackResponsePermissions rec)

data TeamPackResponses = TeamPackResponses {
  teamPackResponses :: !([TeamPackResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON TeamPackResponses where
  parseJSON (Object o) = do
    teamPackResponses <- o .: ("team_pack_responses" :: Text)
    pure $ TeamPackResponses {
      teamPackResponses = teamPackResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TeamPackResponses where
  toJSON TeamPackResponses{..} = object $
    [ "tag" .= ("TeamPackResponses" :: Text)
    , "team_pack_responses" .= teamPackResponses
    ]


instance Eq TeamPackResponses where
  (==) a b = teamPackResponses a == teamPackResponses b

instance Show TeamPackResponses where
    show rec = "teamPackResponses: " <> show (teamPackResponses rec)
-- footer