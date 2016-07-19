{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.Sanitized.User where


import LN.T.User
import LN.T.Like
import LN.T.Star
import LN.T.Profile


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

data UserSanitizedPackResponse = UserSanitizedPackResponse {
  userSanitizedPackResponseUser :: !(UserSanitizedResponse),
  userSanitizedPackResponseUserId :: !(Int64),
  userSanitizedPackResponseProfile :: !(ProfileResponse),
  userSanitizedPackResponseProfileId :: !(Int64),
  userSanitizedPackResponseStat :: !(UserSanitizedStatResponse),
  userSanitizedPackResponseLike :: !((Maybe LikeResponse)),
  userSanitizedPackResponseStar :: !((Maybe StarResponse))
}  deriving (Generic,Typeable,NFData)


instance FromJSON UserSanitizedPackResponse where
  parseJSON (Object o) = do
    userSanitizedPackResponseUser <- o .: ("user" :: Text)
    userSanitizedPackResponseUserId <- o .: ("user_id" :: Text)
    userSanitizedPackResponseProfile <- o .: ("profile" :: Text)
    userSanitizedPackResponseProfileId <- o .: ("profile_id" :: Text)
    userSanitizedPackResponseStat <- o .: ("stat" :: Text)
    userSanitizedPackResponseLike <- o .: ("like" :: Text)
    userSanitizedPackResponseStar <- o .: ("star" :: Text)
    pure $ UserSanitizedPackResponse {
      userSanitizedPackResponseUser = userSanitizedPackResponseUser,
      userSanitizedPackResponseUserId = userSanitizedPackResponseUserId,
      userSanitizedPackResponseProfile = userSanitizedPackResponseProfile,
      userSanitizedPackResponseProfileId = userSanitizedPackResponseProfileId,
      userSanitizedPackResponseStat = userSanitizedPackResponseStat,
      userSanitizedPackResponseLike = userSanitizedPackResponseLike,
      userSanitizedPackResponseStar = userSanitizedPackResponseStar
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON UserSanitizedPackResponse where
  toJSON UserSanitizedPackResponse{..} = object $
    [ "tag" .= ("UserSanitizedPackResponse" :: Text)
    , "user" .= userSanitizedPackResponseUser
    , "user_id" .= userSanitizedPackResponseUserId
    , "profile" .= userSanitizedPackResponseProfile
    , "profile_id" .= userSanitizedPackResponseProfileId
    , "stat" .= userSanitizedPackResponseStat
    , "like" .= userSanitizedPackResponseLike
    , "star" .= userSanitizedPackResponseStar
    ]


instance Eq UserSanitizedPackResponse where
  (==) a b = userSanitizedPackResponseUser a == userSanitizedPackResponseUser b && userSanitizedPackResponseUserId a == userSanitizedPackResponseUserId b && userSanitizedPackResponseProfile a == userSanitizedPackResponseProfile b && userSanitizedPackResponseProfileId a == userSanitizedPackResponseProfileId b && userSanitizedPackResponseStat a == userSanitizedPackResponseStat b && userSanitizedPackResponseLike a == userSanitizedPackResponseLike b && userSanitizedPackResponseStar a == userSanitizedPackResponseStar b

instance Show UserSanitizedPackResponse where
    show rec = "userSanitizedPackResponseUser: " <> show (userSanitizedPackResponseUser rec) <> ", " <> "userSanitizedPackResponseUserId: " <> show (userSanitizedPackResponseUserId rec) <> ", " <> "userSanitizedPackResponseProfile: " <> show (userSanitizedPackResponseProfile rec) <> ", " <> "userSanitizedPackResponseProfileId: " <> show (userSanitizedPackResponseProfileId rec) <> ", " <> "userSanitizedPackResponseStat: " <> show (userSanitizedPackResponseStat rec) <> ", " <> "userSanitizedPackResponseLike: " <> show (userSanitizedPackResponseLike rec) <> ", " <> "userSanitizedPackResponseStar: " <> show (userSanitizedPackResponseStar rec)

data UserSanitizedPackResponses = UserSanitizedPackResponses {
  userSanitizedPackResponses :: !([UserSanitizedPackResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON UserSanitizedPackResponses where
  parseJSON (Object o) = do
    userSanitizedPackResponses <- o .: ("user_sanitized_pack_responses" :: Text)
    pure $ UserSanitizedPackResponses {
      userSanitizedPackResponses = userSanitizedPackResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON UserSanitizedPackResponses where
  toJSON UserSanitizedPackResponses{..} = object $
    [ "tag" .= ("UserSanitizedPackResponses" :: Text)
    , "user_sanitized_pack_responses" .= userSanitizedPackResponses
    ]


instance Eq UserSanitizedPackResponses where
  (==) a b = userSanitizedPackResponses a == userSanitizedPackResponses b

instance Show UserSanitizedPackResponses where
    show rec = "userSanitizedPackResponses: " <> show (userSanitizedPackResponses rec)
-- footer