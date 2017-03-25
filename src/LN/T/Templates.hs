{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Templates where


import LN.T.Resource
import LN.T.Leuron
import LN.T.LeuronTraining
import LN.T.Bucket


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

data Templates = Templates {
  resourceRequest :: !(ResourceRequest),
  leuronRequest :: !(LeuronRequest),
  leuronTrainingRequest :: !(LeuronTrainingRequest),
  bucketRequest :: !(BucketRequest)
}  deriving (Generic,Typeable,NFData)


instance FromJSON Templates where
  parseJSON (Object o) = do
    resourceRequest <- o .: ("resource_request" :: Text)
    leuronRequest <- o .: ("leuron_request" :: Text)
    leuronTrainingRequest <- o .: ("leuron_training_request" :: Text)
    bucketRequest <- o .: ("bucket_request" :: Text)
    pure $ Templates {
      resourceRequest = resourceRequest,
      leuronRequest = leuronRequest,
      leuronTrainingRequest = leuronTrainingRequest,
      bucketRequest = bucketRequest
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Templates where
  toJSON Templates{..} = object $
    [ "tag" .= ("Templates" :: Text)
    , "resource_request" .= resourceRequest
    , "leuron_request" .= leuronRequest
    , "leuron_training_request" .= leuronTrainingRequest
    , "bucket_request" .= bucketRequest
    ]


instance Eq Templates where
  (==) a b = resourceRequest a == resourceRequest b && leuronRequest a == leuronRequest b && leuronTrainingRequest a == leuronTrainingRequest b && bucketRequest a == bucketRequest b

instance Show Templates where
    show rec = "resourceRequest: " <> show (resourceRequest rec) <> ", " <> "leuronRequest: " <> show (leuronRequest rec) <> ", " <> "leuronTrainingRequest: " <> show (leuronTrainingRequest rec) <> ", " <> "bucketRequest: " <> show (bucketRequest rec)
-- footer