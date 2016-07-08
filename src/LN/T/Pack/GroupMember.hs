{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.GroupMember where




import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)

instance FromJSON GroupMemberPackResponse where
  parseJSON (Object o) = do
    groupMemberPackResponseUser <- o .: ("user" :: Text)
    groupMemberPackResponseUserId <- o .: ("user_id" :: Text)
    groupMemberPackResponseGroupMember <- o .: ("group_member" :: Text)
    groupMemberPackResponseGroupMemberId <- o .: ("group_member_id" :: Text)
    groupMemberPackResponseIsOwner <- o .: ("is_owner" :: Text)
    pure $ GroupMemberPackResponse {
      groupMemberPackResponseUser = groupMemberPackResponseUser,
      groupMemberPackResponseUserId = groupMemberPackResponseUserId,
      groupMemberPackResponseGroupMember = groupMemberPackResponseGroupMember,
      groupMemberPackResponseGroupMemberId = groupMemberPackResponseGroupMemberId,
      groupMemberPackResponseIsOwner = groupMemberPackResponseIsOwner
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GroupMemberPackResponse where
  toJSON GroupMemberPackResponse{..} = object $
    [ "tag" .= ("GroupMemberPackResponse" :: Text)
    , "user" .= groupMemberPackResponseUser
    , "user_id" .= groupMemberPackResponseUserId
    , "group_member" .= groupMemberPackResponseGroupMember
    , "group_member_id" .= groupMemberPackResponseGroupMemberId
    , "is_owner" .= groupMemberPackResponseIsOwner
    ]


instance Eq GroupMemberPackResponse where
  (==) a b = groupMemberPackResponseUser a == groupMemberPackResponseUser b && groupMemberPackResponseUserId a == groupMemberPackResponseUserId b && groupMemberPackResponseGroupMember a == groupMemberPackResponseGroupMember b && groupMemberPackResponseGroupMemberId a == groupMemberPackResponseGroupMemberId b && groupMemberPackResponseIsOwner a == groupMemberPackResponseIsOwner b

instance Show GroupMemberPackResponse where
    show rec = "groupMemberPackResponseUser: " <> show (groupMemberPackResponseUser rec) <> ", " <> "groupMemberPackResponseUserId: " <> show (groupMemberPackResponseUserId rec) <> ", " <> "groupMemberPackResponseGroupMember: " <> show (groupMemberPackResponseGroupMember rec) <> ", " <> "groupMemberPackResponseGroupMemberId: " <> show (groupMemberPackResponseGroupMemberId rec) <> ", " <> "groupMemberPackResponseIsOwner: " <> show (groupMemberPackResponseIsOwner rec)

instance FromJSON GroupMemberPackResponses where
  parseJSON (Object o) = do
    groupMemberPackResponses <- o .: ("group_member_pack_responses" :: Text)
    pure $ GroupMemberPackResponses {
      groupMemberPackResponses = groupMemberPackResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON GroupMemberPackResponses where
  toJSON GroupMemberPackResponses{..} = object $
    [ "tag" .= ("GroupMemberPackResponses" :: Text)
    , "group_member_pack_responses" .= groupMemberPackResponses
    ]


instance Eq GroupMemberPackResponses where
  (==) a b = groupMemberPackResponses a == groupMemberPackResponses b

instance Show GroupMemberPackResponses where
    show rec = "groupMemberPackResponses: " <> show (groupMemberPackResponses rec)
-- footer