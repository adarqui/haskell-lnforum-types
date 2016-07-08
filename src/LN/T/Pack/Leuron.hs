{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.Leuron where




import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)

instance FromJSON LeuronPackResponse where
  parseJSON (Object o) = do
    leuronPackResponseLeuron <- o .: ("leuron" :: Text)
    leuronPackResponseLeuronId <- o .: ("leuron_id" :: Text)
    leuronPackResponseUser <- o .: ("user" :: Text)
    leuronPackResponseUserId <- o .: ("user_id" :: Text)
    leuronPackResponseTraining <- o .: ("training" :: Text)
    leuronPackResponseStat <- o .: ("stat" :: Text)
    leuronPackResponseLike <- o .: ("like" :: Text)
    leuronPackResponseStar <- o .: ("star" :: Text)
    leuronPackResponsePermissions <- o .: ("permissions" :: Text)
    pure $ LeuronPackResponse {
      leuronPackResponseLeuron = leuronPackResponseLeuron,
      leuronPackResponseLeuronId = leuronPackResponseLeuronId,
      leuronPackResponseUser = leuronPackResponseUser,
      leuronPackResponseUserId = leuronPackResponseUserId,
      leuronPackResponseTraining = leuronPackResponseTraining,
      leuronPackResponseStat = leuronPackResponseStat,
      leuronPackResponseLike = leuronPackResponseLike,
      leuronPackResponseStar = leuronPackResponseStar,
      leuronPackResponsePermissions = leuronPackResponsePermissions
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronPackResponse where
  toJSON LeuronPackResponse{..} = object $
    [ "tag" .= ("LeuronPackResponse" :: Text)
    , "leuron" .= leuronPackResponseLeuron
    , "leuron_id" .= leuronPackResponseLeuronId
    , "user" .= leuronPackResponseUser
    , "user_id" .= leuronPackResponseUserId
    , "training" .= leuronPackResponseTraining
    , "stat" .= leuronPackResponseStat
    , "like" .= leuronPackResponseLike
    , "star" .= leuronPackResponseStar
    , "permissions" .= leuronPackResponsePermissions
    ]


instance Eq LeuronPackResponse where
  (==) a b = leuronPackResponseLeuron a == leuronPackResponseLeuron b && leuronPackResponseLeuronId a == leuronPackResponseLeuronId b && leuronPackResponseUser a == leuronPackResponseUser b && leuronPackResponseUserId a == leuronPackResponseUserId b && leuronPackResponseTraining a == leuronPackResponseTraining b && leuronPackResponseStat a == leuronPackResponseStat b && leuronPackResponseLike a == leuronPackResponseLike b && leuronPackResponseStar a == leuronPackResponseStar b && leuronPackResponsePermissions a == leuronPackResponsePermissions b

instance Show LeuronPackResponse where
    show rec = "leuronPackResponseLeuron: " <> show (leuronPackResponseLeuron rec) <> ", " <> "leuronPackResponseLeuronId: " <> show (leuronPackResponseLeuronId rec) <> ", " <> "leuronPackResponseUser: " <> show (leuronPackResponseUser rec) <> ", " <> "leuronPackResponseUserId: " <> show (leuronPackResponseUserId rec) <> ", " <> "leuronPackResponseTraining: " <> show (leuronPackResponseTraining rec) <> ", " <> "leuronPackResponseStat: " <> show (leuronPackResponseStat rec) <> ", " <> "leuronPackResponseLike: " <> show (leuronPackResponseLike rec) <> ", " <> "leuronPackResponseStar: " <> show (leuronPackResponseStar rec) <> ", " <> "leuronPackResponsePermissions: " <> show (leuronPackResponsePermissions rec)

instance FromJSON LeuronPackResponses where
  parseJSON (Object o) = do
    leuronPackResponses <- o .: ("leuron_pack_responses" :: Text)
    pure $ LeuronPackResponses {
      leuronPackResponses = leuronPackResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronPackResponses where
  toJSON LeuronPackResponses{..} = object $
    [ "tag" .= ("LeuronPackResponses" :: Text)
    , "leuron_pack_responses" .= leuronPackResponses
    ]


instance Eq LeuronPackResponses where
  (==) a b = leuronPackResponses a == leuronPackResponses b

instance Show LeuronPackResponses where
    show rec = "leuronPackResponses: " <> show (leuronPackResponses rec)
-- footer