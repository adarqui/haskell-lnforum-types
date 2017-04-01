{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.Bucket where


import LN.T.Bucket
import LN.T.User


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

data BucketPackResponse = BucketPackResponse {
  bucketPackResponseBucket :: !(BucketResponse),
  bucketPackResponseBucketId :: !(Int64),
  bucketPackResponseUser :: !(UserSanitizedResponse),
  bucketPackResponseUserId :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON BucketPackResponse where
  parseJSON (Object o) = do
    bucketPackResponseBucket <- o .: ("bucket" :: Text)
    bucketPackResponseBucketId <- o .: ("bucket_id" :: Text)
    bucketPackResponseUser <- o .: ("user" :: Text)
    bucketPackResponseUserId <- o .: ("user_id" :: Text)
    pure $ BucketPackResponse {
      bucketPackResponseBucket = bucketPackResponseBucket,
      bucketPackResponseBucketId = bucketPackResponseBucketId,
      bucketPackResponseUser = bucketPackResponseUser,
      bucketPackResponseUserId = bucketPackResponseUserId
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BucketPackResponse where
  toJSON BucketPackResponse{..} = object $
    [ "tag" .= ("BucketPackResponse" :: Text)
    , "bucket" .= bucketPackResponseBucket
    , "bucket_id" .= bucketPackResponseBucketId
    , "user" .= bucketPackResponseUser
    , "user_id" .= bucketPackResponseUserId
    ]


instance Eq BucketPackResponse where
  (==) a b = bucketPackResponseBucket a == bucketPackResponseBucket b && bucketPackResponseBucketId a == bucketPackResponseBucketId b && bucketPackResponseUser a == bucketPackResponseUser b && bucketPackResponseUserId a == bucketPackResponseUserId b

instance Show BucketPackResponse where
    show rec = "bucketPackResponseBucket: " <> show (bucketPackResponseBucket rec) <> ", " <> "bucketPackResponseBucketId: " <> show (bucketPackResponseBucketId rec) <> ", " <> "bucketPackResponseUser: " <> show (bucketPackResponseUser rec) <> ", " <> "bucketPackResponseUserId: " <> show (bucketPackResponseUserId rec)

data BucketPackResponses = BucketPackResponses {
  bucketPackResponses :: !([BucketPackResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON BucketPackResponses where
  parseJSON (Object o) = do
    bucketPackResponses <- o .: ("bucket_pack_responses" :: Text)
    pure $ BucketPackResponses {
      bucketPackResponses = bucketPackResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BucketPackResponses where
  toJSON BucketPackResponses{..} = object $
    [ "tag" .= ("BucketPackResponses" :: Text)
    , "bucket_pack_responses" .= bucketPackResponses
    ]


instance Eq BucketPackResponses where
  (==) a b = bucketPackResponses a == bucketPackResponses b

instance Show BucketPackResponses where
    show rec = "bucketPackResponses: " <> show (bucketPackResponses rec)
-- footer