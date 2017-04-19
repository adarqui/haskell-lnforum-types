{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.LeuronNode where


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

data LeuronNodeRequest = LeuronNodeRequest {
  leuronNodeRequestRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronNodeRequest where
  parseJSON (Object o) = do
    leuronNodeRequestRequestGuard <- o .: ("request_guard" :: Text)
    pure $ LeuronNodeRequest {
      leuronNodeRequestRequestGuard = leuronNodeRequestRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronNodeRequest where
  toJSON LeuronNodeRequest{..} = object $
    [ "tag" .= ("LeuronNodeRequest" :: Text)
    , "request_guard" .= leuronNodeRequestRequestGuard
    ]


instance Eq LeuronNodeRequest where
  (==) a b = leuronNodeRequestRequestGuard a == leuronNodeRequestRequestGuard b

instance Show LeuronNodeRequest where
    show rec = "leuronNodeRequestRequestGuard: " <> show (leuronNodeRequestRequestGuard rec)

data LeuronNodeResponse = LeuronNodeResponse {
  leuronNodeResponseId :: !(Int64),
  leuronNodeResponseUserId :: !(Int64),
  leuronNodeResponseLeuronId :: !(Int64),
  leuronNodeResponseTrainingNode :: !(TrainingNode),
  leuronNodeResponseActive :: !(Bool),
  leuronNodeResponseGuard :: !(Int),
  leuronNodeResponseCreatedAt :: !((Maybe UTCTime)),
  leuronNodeResponseModifiedAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronNodeResponse where
  parseJSON (Object o) = do
    leuronNodeResponseId <- o .: ("id" :: Text)
    leuronNodeResponseUserId <- o .: ("user_id" :: Text)
    leuronNodeResponseLeuronId <- o .: ("leuron_id" :: Text)
    leuronNodeResponseTrainingNode <- o .: ("training_node" :: Text)
    leuronNodeResponseActive <- o .: ("active" :: Text)
    leuronNodeResponseGuard <- o .: ("guard" :: Text)
    leuronNodeResponseCreatedAt <- o .: ("created_at" :: Text)
    leuronNodeResponseModifiedAt <- o .: ("modified_at" :: Text)
    pure $ LeuronNodeResponse {
      leuronNodeResponseId = leuronNodeResponseId,
      leuronNodeResponseUserId = leuronNodeResponseUserId,
      leuronNodeResponseLeuronId = leuronNodeResponseLeuronId,
      leuronNodeResponseTrainingNode = leuronNodeResponseTrainingNode,
      leuronNodeResponseActive = leuronNodeResponseActive,
      leuronNodeResponseGuard = leuronNodeResponseGuard,
      leuronNodeResponseCreatedAt = leuronNodeResponseCreatedAt,
      leuronNodeResponseModifiedAt = leuronNodeResponseModifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronNodeResponse where
  toJSON LeuronNodeResponse{..} = object $
    [ "tag" .= ("LeuronNodeResponse" :: Text)
    , "id" .= leuronNodeResponseId
    , "user_id" .= leuronNodeResponseUserId
    , "leuron_id" .= leuronNodeResponseLeuronId
    , "training_node" .= leuronNodeResponseTrainingNode
    , "active" .= leuronNodeResponseActive
    , "guard" .= leuronNodeResponseGuard
    , "created_at" .= leuronNodeResponseCreatedAt
    , "modified_at" .= leuronNodeResponseModifiedAt
    ]


instance Eq LeuronNodeResponse where
  (==) a b = leuronNodeResponseId a == leuronNodeResponseId b && leuronNodeResponseUserId a == leuronNodeResponseUserId b && leuronNodeResponseLeuronId a == leuronNodeResponseLeuronId b && leuronNodeResponseTrainingNode a == leuronNodeResponseTrainingNode b && leuronNodeResponseActive a == leuronNodeResponseActive b && leuronNodeResponseGuard a == leuronNodeResponseGuard b && leuronNodeResponseCreatedAt a == leuronNodeResponseCreatedAt b && leuronNodeResponseModifiedAt a == leuronNodeResponseModifiedAt b

instance Show LeuronNodeResponse where
    show rec = "leuronNodeResponseId: " <> show (leuronNodeResponseId rec) <> ", " <> "leuronNodeResponseUserId: " <> show (leuronNodeResponseUserId rec) <> ", " <> "leuronNodeResponseLeuronId: " <> show (leuronNodeResponseLeuronId rec) <> ", " <> "leuronNodeResponseTrainingNode: " <> show (leuronNodeResponseTrainingNode rec) <> ", " <> "leuronNodeResponseActive: " <> show (leuronNodeResponseActive rec) <> ", " <> "leuronNodeResponseGuard: " <> show (leuronNodeResponseGuard rec) <> ", " <> "leuronNodeResponseCreatedAt: " <> show (leuronNodeResponseCreatedAt rec) <> ", " <> "leuronNodeResponseModifiedAt: " <> show (leuronNodeResponseModifiedAt rec)

data LeuronNodeResponses = LeuronNodeResponses {
  leuronNodeResponses :: !([LeuronNodeResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronNodeResponses where
  parseJSON (Object o) = do
    leuronNodeResponses <- o .: ("leuron_node_responses" :: Text)
    pure $ LeuronNodeResponses {
      leuronNodeResponses = leuronNodeResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronNodeResponses where
  toJSON LeuronNodeResponses{..} = object $
    [ "tag" .= ("LeuronNodeResponses" :: Text)
    , "leuron_node_responses" .= leuronNodeResponses
    ]


instance Eq LeuronNodeResponses where
  (==) a b = leuronNodeResponses a == leuronNodeResponses b

instance Show LeuronNodeResponses where
    show rec = "leuronNodeResponses: " <> show (leuronNodeResponses rec)
-- footer