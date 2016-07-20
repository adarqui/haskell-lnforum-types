{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Bucket where





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

data BucketRequest = BucketRequest {
  bucketRequestDisplayName :: !(Text),
  bucketRequestDescription :: !((Maybe Text)),
  bucketRequestScoreLo :: !(Int),
  bucketRequestScoreHi :: !(Int),
  bucketRequestLeurons :: !([Int64]),
  bucketRequestResources :: !([Int64]),
  bucketRequestCategories :: !([Text]),
  bucketRequestFilters :: !([Int64]),
  bucketRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON BucketRequest where
  parseJSON (Object o) = do
    bucketRequestDisplayName <- o .: ("display_name" :: Text)
    bucketRequestDescription <- o .: ("description" :: Text)
    bucketRequestScoreLo <- o .: ("score_lo" :: Text)
    bucketRequestScoreHi <- o .: ("score_hi" :: Text)
    bucketRequestLeurons <- o .: ("leurons" :: Text)
    bucketRequestResources <- o .: ("resources" :: Text)
    bucketRequestCategories <- o .: ("categories" :: Text)
    bucketRequestFilters <- o .: ("filters" :: Text)
    bucketRequestGuard <- o .: ("guard" :: Text)
    pure $ BucketRequest {
      bucketRequestDisplayName = bucketRequestDisplayName,
      bucketRequestDescription = bucketRequestDescription,
      bucketRequestScoreLo = bucketRequestScoreLo,
      bucketRequestScoreHi = bucketRequestScoreHi,
      bucketRequestLeurons = bucketRequestLeurons,
      bucketRequestResources = bucketRequestResources,
      bucketRequestCategories = bucketRequestCategories,
      bucketRequestFilters = bucketRequestFilters,
      bucketRequestGuard = bucketRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BucketRequest where
  toJSON BucketRequest{..} = object $
    [ "tag" .= ("BucketRequest" :: Text)
    , "display_name" .= bucketRequestDisplayName
    , "description" .= bucketRequestDescription
    , "score_lo" .= bucketRequestScoreLo
    , "score_hi" .= bucketRequestScoreHi
    , "leurons" .= bucketRequestLeurons
    , "resources" .= bucketRequestResources
    , "categories" .= bucketRequestCategories
    , "filters" .= bucketRequestFilters
    , "guard" .= bucketRequestGuard
    ]


instance Eq BucketRequest where
  (==) a b = bucketRequestDisplayName a == bucketRequestDisplayName b && bucketRequestDescription a == bucketRequestDescription b && bucketRequestScoreLo a == bucketRequestScoreLo b && bucketRequestScoreHi a == bucketRequestScoreHi b && bucketRequestLeurons a == bucketRequestLeurons b && bucketRequestResources a == bucketRequestResources b && bucketRequestCategories a == bucketRequestCategories b && bucketRequestFilters a == bucketRequestFilters b && bucketRequestGuard a == bucketRequestGuard b

instance Show BucketRequest where
    show rec = "bucketRequestDisplayName: " <> show (bucketRequestDisplayName rec) <> ", " <> "bucketRequestDescription: " <> show (bucketRequestDescription rec) <> ", " <> "bucketRequestScoreLo: " <> show (bucketRequestScoreLo rec) <> ", " <> "bucketRequestScoreHi: " <> show (bucketRequestScoreHi rec) <> ", " <> "bucketRequestLeurons: " <> show (bucketRequestLeurons rec) <> ", " <> "bucketRequestResources: " <> show (bucketRequestResources rec) <> ", " <> "bucketRequestCategories: " <> show (bucketRequestCategories rec) <> ", " <> "bucketRequestFilters: " <> show (bucketRequestFilters rec) <> ", " <> "bucketRequestGuard: " <> show (bucketRequestGuard rec)

data BucketResponse = BucketResponse {
  bucketResponseId :: !(Int64),
  bucketResponseUserId :: !(Int64),
  bucketResponseName :: !(Text),
  bucketResponseDisplayName :: !(Text),
  bucketResponseDescription :: !((Maybe Text)),
  bucketResponseScoreLo :: !(Int),
  bucketResponseScoreHi :: !(Int),
  bucketResponseLeurons :: !([Int64]),
  bucketResponseResources :: !([Int64]),
  bucketResponseCategories :: !([Text]),
  bucketResponseFilters :: !([Int64]),
  bucketResponseActive :: !(Bool),
  bucketResponseGuard :: !(Int),
  bucketResponseCreatedAt :: !((Maybe UTCTime)),
  bucketResponseModifiedAt :: !((Maybe UTCTime)),
  bucketResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON BucketResponse where
  parseJSON (Object o) = do
    bucketResponseId <- o .: ("id" :: Text)
    bucketResponseUserId <- o .: ("user_id" :: Text)
    bucketResponseName <- o .: ("name" :: Text)
    bucketResponseDisplayName <- o .: ("display_name" :: Text)
    bucketResponseDescription <- o .: ("description" :: Text)
    bucketResponseScoreLo <- o .: ("score_lo" :: Text)
    bucketResponseScoreHi <- o .: ("score_hi" :: Text)
    bucketResponseLeurons <- o .: ("leurons" :: Text)
    bucketResponseResources <- o .: ("resources" :: Text)
    bucketResponseCategories <- o .: ("categories" :: Text)
    bucketResponseFilters <- o .: ("filters" :: Text)
    bucketResponseActive <- o .: ("active" :: Text)
    bucketResponseGuard <- o .: ("guard" :: Text)
    bucketResponseCreatedAt <- o .: ("created_at" :: Text)
    bucketResponseModifiedAt <- o .: ("modified_at" :: Text)
    bucketResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ BucketResponse {
      bucketResponseId = bucketResponseId,
      bucketResponseUserId = bucketResponseUserId,
      bucketResponseName = bucketResponseName,
      bucketResponseDisplayName = bucketResponseDisplayName,
      bucketResponseDescription = bucketResponseDescription,
      bucketResponseScoreLo = bucketResponseScoreLo,
      bucketResponseScoreHi = bucketResponseScoreHi,
      bucketResponseLeurons = bucketResponseLeurons,
      bucketResponseResources = bucketResponseResources,
      bucketResponseCategories = bucketResponseCategories,
      bucketResponseFilters = bucketResponseFilters,
      bucketResponseActive = bucketResponseActive,
      bucketResponseGuard = bucketResponseGuard,
      bucketResponseCreatedAt = bucketResponseCreatedAt,
      bucketResponseModifiedAt = bucketResponseModifiedAt,
      bucketResponseActivityAt = bucketResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BucketResponse where
  toJSON BucketResponse{..} = object $
    [ "tag" .= ("BucketResponse" :: Text)
    , "id" .= bucketResponseId
    , "user_id" .= bucketResponseUserId
    , "name" .= bucketResponseName
    , "display_name" .= bucketResponseDisplayName
    , "description" .= bucketResponseDescription
    , "score_lo" .= bucketResponseScoreLo
    , "score_hi" .= bucketResponseScoreHi
    , "leurons" .= bucketResponseLeurons
    , "resources" .= bucketResponseResources
    , "categories" .= bucketResponseCategories
    , "filters" .= bucketResponseFilters
    , "active" .= bucketResponseActive
    , "guard" .= bucketResponseGuard
    , "created_at" .= bucketResponseCreatedAt
    , "modified_at" .= bucketResponseModifiedAt
    , "activity_at" .= bucketResponseActivityAt
    ]


instance Eq BucketResponse where
  (==) a b = bucketResponseId a == bucketResponseId b && bucketResponseUserId a == bucketResponseUserId b && bucketResponseName a == bucketResponseName b && bucketResponseDisplayName a == bucketResponseDisplayName b && bucketResponseDescription a == bucketResponseDescription b && bucketResponseScoreLo a == bucketResponseScoreLo b && bucketResponseScoreHi a == bucketResponseScoreHi b && bucketResponseLeurons a == bucketResponseLeurons b && bucketResponseResources a == bucketResponseResources b && bucketResponseCategories a == bucketResponseCategories b && bucketResponseFilters a == bucketResponseFilters b && bucketResponseActive a == bucketResponseActive b && bucketResponseGuard a == bucketResponseGuard b && bucketResponseCreatedAt a == bucketResponseCreatedAt b && bucketResponseModifiedAt a == bucketResponseModifiedAt b && bucketResponseActivityAt a == bucketResponseActivityAt b

instance Show BucketResponse where
    show rec = "bucketResponseId: " <> show (bucketResponseId rec) <> ", " <> "bucketResponseUserId: " <> show (bucketResponseUserId rec) <> ", " <> "bucketResponseName: " <> show (bucketResponseName rec) <> ", " <> "bucketResponseDisplayName: " <> show (bucketResponseDisplayName rec) <> ", " <> "bucketResponseDescription: " <> show (bucketResponseDescription rec) <> ", " <> "bucketResponseScoreLo: " <> show (bucketResponseScoreLo rec) <> ", " <> "bucketResponseScoreHi: " <> show (bucketResponseScoreHi rec) <> ", " <> "bucketResponseLeurons: " <> show (bucketResponseLeurons rec) <> ", " <> "bucketResponseResources: " <> show (bucketResponseResources rec) <> ", " <> "bucketResponseCategories: " <> show (bucketResponseCategories rec) <> ", " <> "bucketResponseFilters: " <> show (bucketResponseFilters rec) <> ", " <> "bucketResponseActive: " <> show (bucketResponseActive rec) <> ", " <> "bucketResponseGuard: " <> show (bucketResponseGuard rec) <> ", " <> "bucketResponseCreatedAt: " <> show (bucketResponseCreatedAt rec) <> ", " <> "bucketResponseModifiedAt: " <> show (bucketResponseModifiedAt rec) <> ", " <> "bucketResponseActivityAt: " <> show (bucketResponseActivityAt rec)

data BucketResponses = BucketResponses {
  bucketResponses :: !([BucketResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON BucketResponses where
  parseJSON (Object o) = do
    bucketResponses <- o .: ("bucket_responses" :: Text)
    pure $ BucketResponses {
      bucketResponses = bucketResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BucketResponses where
  toJSON BucketResponses{..} = object $
    [ "tag" .= ("BucketResponses" :: Text)
    , "bucket_responses" .= bucketResponses
    ]


instance Eq BucketResponses where
  (==) a b = bucketResponses a == bucketResponses b

instance Show BucketResponses where
    show rec = "bucketResponses: " <> show (bucketResponses rec)
-- footer