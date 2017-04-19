{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.BucketNode where


import LN.T.Training


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

data BucketNodeRequest = BucketNodeRequest {
  bucketNodeRequestRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON BucketNodeRequest where
  parseJSON (Object o) = do
    bucketNodeRequestRequestGuard <- o .: ("request_guard" :: Text)
    pure $ BucketNodeRequest {
      bucketNodeRequestRequestGuard = bucketNodeRequestRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BucketNodeRequest where
  toJSON BucketNodeRequest{..} = object $
    [ "tag" .= ("BucketNodeRequest" :: Text)
    , "request_guard" .= bucketNodeRequestRequestGuard
    ]


instance Eq BucketNodeRequest where
  (==) a b = bucketNodeRequestRequestGuard a == bucketNodeRequestRequestGuard b

instance Show BucketNodeRequest where
    show rec = "bucketNodeRequestRequestGuard: " <> show (bucketNodeRequestRequestGuard rec)

data BucketNodeResponse = BucketNodeResponse {
  bucketNodeResponseId :: !(Int64),
  bucketNodeResponseUserId :: !(Int64),
  bucketNodeResponseBucketId :: !(Int64),
  bucketNodeResponseBucketTrainingId :: !(Int64),
  bucketNodeResponseLeuronId :: !(Int64),
  bucketNodeResponseTimeLimit :: !(Int64),
  bucketNodeResponseTimeLimitExceeded :: !(Int64),
  bucketNodeResponseStyle :: !(Text),
  bucketNodeResponseActive :: !(Bool),
  bucketNodeResponseGuard :: !(Int),
  bucketNodeResponseCreatedAt :: !((Maybe UTCTime)),
  bucketNodeResponseModifiedAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON BucketNodeResponse where
  parseJSON (Object o) = do
    bucketNodeResponseId <- o .: ("id" :: Text)
    bucketNodeResponseUserId <- o .: ("user_id" :: Text)
    bucketNodeResponseBucketId <- o .: ("bucket_id" :: Text)
    bucketNodeResponseBucketTrainingId <- o .: ("bucket_training_id" :: Text)
    bucketNodeResponseLeuronId <- o .: ("leuron_id" :: Text)
    bucketNodeResponseTimeLimit <- o .: ("time_limit" :: Text)
    bucketNodeResponseTimeLimitExceeded <- o .: ("time_limit_exceeded" :: Text)
    bucketNodeResponseStyle <- o .: ("style" :: Text)
    bucketNodeResponseActive <- o .: ("active" :: Text)
    bucketNodeResponseGuard <- o .: ("guard" :: Text)
    bucketNodeResponseCreatedAt <- o .: ("created_at" :: Text)
    bucketNodeResponseModifiedAt <- o .: ("modified_at" :: Text)
    pure $ BucketNodeResponse {
      bucketNodeResponseId = bucketNodeResponseId,
      bucketNodeResponseUserId = bucketNodeResponseUserId,
      bucketNodeResponseBucketId = bucketNodeResponseBucketId,
      bucketNodeResponseBucketTrainingId = bucketNodeResponseBucketTrainingId,
      bucketNodeResponseLeuronId = bucketNodeResponseLeuronId,
      bucketNodeResponseTimeLimit = bucketNodeResponseTimeLimit,
      bucketNodeResponseTimeLimitExceeded = bucketNodeResponseTimeLimitExceeded,
      bucketNodeResponseStyle = bucketNodeResponseStyle,
      bucketNodeResponseActive = bucketNodeResponseActive,
      bucketNodeResponseGuard = bucketNodeResponseGuard,
      bucketNodeResponseCreatedAt = bucketNodeResponseCreatedAt,
      bucketNodeResponseModifiedAt = bucketNodeResponseModifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BucketNodeResponse where
  toJSON BucketNodeResponse{..} = object $
    [ "tag" .= ("BucketNodeResponse" :: Text)
    , "id" .= bucketNodeResponseId
    , "user_id" .= bucketNodeResponseUserId
    , "bucket_id" .= bucketNodeResponseBucketId
    , "bucket_training_id" .= bucketNodeResponseBucketTrainingId
    , "leuron_id" .= bucketNodeResponseLeuronId
    , "time_limit" .= bucketNodeResponseTimeLimit
    , "time_limit_exceeded" .= bucketNodeResponseTimeLimitExceeded
    , "style" .= bucketNodeResponseStyle
    , "active" .= bucketNodeResponseActive
    , "guard" .= bucketNodeResponseGuard
    , "created_at" .= bucketNodeResponseCreatedAt
    , "modified_at" .= bucketNodeResponseModifiedAt
    ]


instance Eq BucketNodeResponse where
  (==) a b = bucketNodeResponseId a == bucketNodeResponseId b && bucketNodeResponseUserId a == bucketNodeResponseUserId b && bucketNodeResponseBucketId a == bucketNodeResponseBucketId b && bucketNodeResponseBucketTrainingId a == bucketNodeResponseBucketTrainingId b && bucketNodeResponseLeuronId a == bucketNodeResponseLeuronId b && bucketNodeResponseTimeLimit a == bucketNodeResponseTimeLimit b && bucketNodeResponseTimeLimitExceeded a == bucketNodeResponseTimeLimitExceeded b && bucketNodeResponseStyle a == bucketNodeResponseStyle b && bucketNodeResponseActive a == bucketNodeResponseActive b && bucketNodeResponseGuard a == bucketNodeResponseGuard b && bucketNodeResponseCreatedAt a == bucketNodeResponseCreatedAt b && bucketNodeResponseModifiedAt a == bucketNodeResponseModifiedAt b

instance Show BucketNodeResponse where
    show rec = "bucketNodeResponseId: " <> show (bucketNodeResponseId rec) <> ", " <> "bucketNodeResponseUserId: " <> show (bucketNodeResponseUserId rec) <> ", " <> "bucketNodeResponseBucketId: " <> show (bucketNodeResponseBucketId rec) <> ", " <> "bucketNodeResponseBucketTrainingId: " <> show (bucketNodeResponseBucketTrainingId rec) <> ", " <> "bucketNodeResponseLeuronId: " <> show (bucketNodeResponseLeuronId rec) <> ", " <> "bucketNodeResponseTimeLimit: " <> show (bucketNodeResponseTimeLimit rec) <> ", " <> "bucketNodeResponseTimeLimitExceeded: " <> show (bucketNodeResponseTimeLimitExceeded rec) <> ", " <> "bucketNodeResponseStyle: " <> show (bucketNodeResponseStyle rec) <> ", " <> "bucketNodeResponseActive: " <> show (bucketNodeResponseActive rec) <> ", " <> "bucketNodeResponseGuard: " <> show (bucketNodeResponseGuard rec) <> ", " <> "bucketNodeResponseCreatedAt: " <> show (bucketNodeResponseCreatedAt rec) <> ", " <> "bucketNodeResponseModifiedAt: " <> show (bucketNodeResponseModifiedAt rec)

data BucketNodeResponses = BucketNodeResponses {
  bucketNodeResponses :: !([BucketNodeResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON BucketNodeResponses where
  parseJSON (Object o) = do
    bucketNodeResponses <- o .: ("bucket_node_responses" :: Text)
    pure $ BucketNodeResponses {
      bucketNodeResponses = bucketNodeResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BucketNodeResponses where
  toJSON BucketNodeResponses{..} = object $
    [ "tag" .= ("BucketNodeResponses" :: Text)
    , "bucket_node_responses" .= bucketNodeResponses
    ]


instance Eq BucketNodeResponses where
  (==) a b = bucketNodeResponses a == bucketNodeResponses b

instance Show BucketNodeResponses where
    show rec = "bucketNodeResponses: " <> show (bucketNodeResponses rec)
-- footer