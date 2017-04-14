{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Id where





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

data IdRequest = IdRequest {
  idRequestTargetId :: !(Int64),
  idRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON IdRequest where
  parseJSON (Object o) = do
    idRequestTargetId <- o .: ("target_id" :: Text)
    idRequestGuard <- o .: ("guard" :: Text)
    pure $ IdRequest {
      idRequestTargetId = idRequestTargetId,
      idRequestGuard = idRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON IdRequest where
  toJSON IdRequest{..} = object $
    [ "tag" .= ("IdRequest" :: Text)
    , "target_id" .= idRequestTargetId
    , "guard" .= idRequestGuard
    ]


instance Eq IdRequest where
  (==) a b = idRequestTargetId a == idRequestTargetId b && idRequestGuard a == idRequestGuard b

instance Show IdRequest where
    show rec = "idRequestTargetId: " <> show (idRequestTargetId rec) <> ", " <> "idRequestGuard: " <> show (idRequestGuard rec)

data IdResponse = IdResponse {
  idResponseId :: !(Int64),
  idResponseUserId :: !(Int64),
  idResponseTargetId :: !(Int64),
  idResponseGuard :: !(Int64),
  idResponseCreatedAt :: !((Maybe UTCTime)),
  idResponseModifiedAt :: !((Maybe UTCTime)),
  idResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON IdResponse where
  parseJSON (Object o) = do
    idResponseId <- o .: ("id" :: Text)
    idResponseUserId <- o .: ("user_id" :: Text)
    idResponseTargetId <- o .: ("target_id" :: Text)
    idResponseGuard <- o .: ("guard" :: Text)
    idResponseCreatedAt <- o .: ("created_at" :: Text)
    idResponseModifiedAt <- o .: ("modified_at" :: Text)
    idResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ IdResponse {
      idResponseId = idResponseId,
      idResponseUserId = idResponseUserId,
      idResponseTargetId = idResponseTargetId,
      idResponseGuard = idResponseGuard,
      idResponseCreatedAt = idResponseCreatedAt,
      idResponseModifiedAt = idResponseModifiedAt,
      idResponseActivityAt = idResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON IdResponse where
  toJSON IdResponse{..} = object $
    [ "tag" .= ("IdResponse" :: Text)
    , "id" .= idResponseId
    , "user_id" .= idResponseUserId
    , "target_id" .= idResponseTargetId
    , "guard" .= idResponseGuard
    , "created_at" .= idResponseCreatedAt
    , "modified_at" .= idResponseModifiedAt
    , "activity_at" .= idResponseActivityAt
    ]


instance Eq IdResponse where
  (==) a b = idResponseId a == idResponseId b && idResponseUserId a == idResponseUserId b && idResponseTargetId a == idResponseTargetId b && idResponseGuard a == idResponseGuard b && idResponseCreatedAt a == idResponseCreatedAt b && idResponseModifiedAt a == idResponseModifiedAt b && idResponseActivityAt a == idResponseActivityAt b

instance Show IdResponse where
    show rec = "idResponseId: " <> show (idResponseId rec) <> ", " <> "idResponseUserId: " <> show (idResponseUserId rec) <> ", " <> "idResponseTargetId: " <> show (idResponseTargetId rec) <> ", " <> "idResponseGuard: " <> show (idResponseGuard rec) <> ", " <> "idResponseCreatedAt: " <> show (idResponseCreatedAt rec) <> ", " <> "idResponseModifiedAt: " <> show (idResponseModifiedAt rec) <> ", " <> "idResponseActivityAt: " <> show (idResponseActivityAt rec)

data IdResponses = IdResponses {
  idResponses :: !([IdResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON IdResponses where
  parseJSON (Object o) = do
    idResponses <- o .: ("id_responses" :: Text)
    pure $ IdResponses {
      idResponses = idResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON IdResponses where
  toJSON IdResponses{..} = object $
    [ "tag" .= ("IdResponses" :: Text)
    , "id_responses" .= idResponses
    ]


instance Eq IdResponses where
  (==) a b = idResponses a == idResponses b

instance Show IdResponses where
    show rec = "idResponses: " <> show (idResponses rec)
-- footer