{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.GlobalGroup where


import LN.T.GlobalGroup
import LN.T.User
import LN.T.Permission


import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Int            (Int64)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)
import           Data.Default

data GlobalGroupPackResponse = GlobalGroupPackResponse {
  globalGroupPackResponseUser :: UserSanitizedResponse,
  globalGroupPackResponseUserId :: Int64,
  globalGroupPackResponseGlobalGroup :: GlobalGroupResponse,
  globalGroupPackResponseGlobalGroupId :: Int64,
  globalGroupPackResponseStat :: GlobalGroupStatResponse,
  globalGroupPackResponsePermissions :: Permissions
}


instance FromJSON GlobalGroupPackResponse where
  parseJSON (Object o) = do
    globalGroupPackResponseUser <- o .: ("user" :: Text)
    globalGroupPackResponseUserId <- o .: ("user_id" :: Text)
    globalGroupPackResponseGlobalGroup <- o .: ("global_group" :: Text)
    globalGroupPackResponseGlobalGroupId <- o .: ("global_group_id" :: Text)
    globalGroupPackResponseStat <- o .: ("stat" :: Text)
    globalGroupPackResponsePermissions <- o .: ("permissions" :: Text)
    pure $ GlobalGroupPackResponse {
      globalGroupPackResponseUser = globalGroupPackResponseUser,
      globalGroupPackResponseUserId = globalGroupPackResponseUserId,
      globalGroupPackResponseGlobalGroup = globalGroupPackResponseGlobalGroup,
      globalGroupPackResponseGlobalGroupId = globalGroupPackResponseGlobalGroupId,
      globalGroupPackResponseStat = globalGroupPackResponseStat,
      globalGroupPackResponsePermissions = globalGroupPackResponsePermissions
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GlobalGroupPackResponse where
  toJSON GlobalGroupPackResponse{..} = object $
    [ "tag" .= ("GlobalGroupPackResponse" :: Text)
    , "user" .= globalGroupPackResponseUser
    , "user_id" .= globalGroupPackResponseUserId
    , "global_group" .= globalGroupPackResponseGlobalGroup
    , "global_group_id" .= globalGroupPackResponseGlobalGroupId
    , "stat" .= globalGroupPackResponseStat
    , "permissions" .= globalGroupPackResponsePermissions
    ]


instance Eq GlobalGroupPackResponse where
  (==) a b = globalGroupPackResponseUser a == globalGroupPackResponseUser b && globalGroupPackResponseUserId a == globalGroupPackResponseUserId b && globalGroupPackResponseGlobalGroup a == globalGroupPackResponseGlobalGroup b && globalGroupPackResponseGlobalGroupId a == globalGroupPackResponseGlobalGroupId b && globalGroupPackResponseStat a == globalGroupPackResponseStat b && globalGroupPackResponsePermissions a == globalGroupPackResponsePermissions b

instance Show GlobalGroupPackResponse where
    show rec = "globalGroupPackResponseUser: " <> show (globalGroupPackResponseUser rec) <> ", " <> "globalGroupPackResponseUserId: " <> show (globalGroupPackResponseUserId rec) <> ", " <> "globalGroupPackResponseGlobalGroup: " <> show (globalGroupPackResponseGlobalGroup rec) <> ", " <> "globalGroupPackResponseGlobalGroupId: " <> show (globalGroupPackResponseGlobalGroupId rec) <> ", " <> "globalGroupPackResponseStat: " <> show (globalGroupPackResponseStat rec) <> ", " <> "globalGroupPackResponsePermissions: " <> show (globalGroupPackResponsePermissions rec)

data GlobalGroupPackResponses = GlobalGroupPackResponses {
  globalGroupPackResponses :: [GlobalGroupPackResponse]
}


instance FromJSON GlobalGroupPackResponses where
  parseJSON (Object o) = do
    globalGroupPackResponses <- o .: ("global_group_pack_responses" :: Text)
    pure $ GlobalGroupPackResponses {
      globalGroupPackResponses = globalGroupPackResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GlobalGroupPackResponses where
  toJSON GlobalGroupPackResponses{..} = object $
    [ "tag" .= ("GlobalGroupPackResponses" :: Text)
    , "global_group_pack_responses" .= globalGroupPackResponses
    ]


instance Eq GlobalGroupPackResponses where
  (==) a b = globalGroupPackResponses a == globalGroupPackResponses b

instance Show GlobalGroupPackResponses where
    show rec = "globalGroupPackResponses: " <> show (globalGroupPackResponses rec)
-- footer