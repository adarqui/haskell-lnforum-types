{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.PmOut where




import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)

instance FromJSON PmOutRequest where
  parseJSON (Object o) = do
    pmOutRequestLabel <- o .: ("label" :: Text)
    pmOutRequestGuard <- o .: ("guard" :: Text)
    pure $ PmOutRequest {
      pmOutRequestLabel = pmOutRequestLabel,
      pmOutRequestGuard = pmOutRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON PmOutRequest where
  toJSON PmOutRequest{..} = object $
    [ "tag" .= ("PmOutRequest" :: Text)
    , "label" .= pmOutRequestLabel
    , "guard" .= pmOutRequestGuard
    ]


instance Eq PmOutRequest where
  (==) a b = pmOutRequestLabel a == pmOutRequestLabel b && pmOutRequestGuard a == pmOutRequestGuard b

instance Show PmOutRequest where
    show rec = "pmOutRequestLabel: " <> show (pmOutRequestLabel rec) <> ", " <> "pmOutRequestGuard: " <> show (pmOutRequestGuard rec)

instance FromJSON PmOutResponse where
  parseJSON (Object o) = do
    pmOutResponseId <- o .: ("id" :: Text)
    pmOutResponsePmId <- o .: ("pm_id" :: Text)
    pmOutResponseUserId <- o .: ("user_id" :: Text)
    pmOutResponseLabel <- o .: ("label" :: Text)
    pmOutResponseIsSaved <- o .: ("is_saved" :: Text)
    pmOutResponseActive <- o .: ("active" :: Text)
    pmOutResponseGuard <- o .: ("guard" :: Text)
    pmOutResponseCreatedAt <- o .: ("created_at" :: Text)
    pmOutResponseModifiedAt <- o .: ("modified_at" :: Text)
    pure $ PmOutResponse {
      pmOutResponseId = pmOutResponseId,
      pmOutResponsePmId = pmOutResponsePmId,
      pmOutResponseUserId = pmOutResponseUserId,
      pmOutResponseLabel = pmOutResponseLabel,
      pmOutResponseIsSaved = pmOutResponseIsSaved,
      pmOutResponseActive = pmOutResponseActive,
      pmOutResponseGuard = pmOutResponseGuard,
      pmOutResponseCreatedAt = pmOutResponseCreatedAt,
      pmOutResponseModifiedAt = pmOutResponseModifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON PmOutResponse where
  toJSON PmOutResponse{..} = object $
    [ "tag" .= ("PmOutResponse" :: Text)
    , "id" .= pmOutResponseId
    , "pm_id" .= pmOutResponsePmId
    , "user_id" .= pmOutResponseUserId
    , "label" .= pmOutResponseLabel
    , "is_saved" .= pmOutResponseIsSaved
    , "active" .= pmOutResponseActive
    , "guard" .= pmOutResponseGuard
    , "created_at" .= pmOutResponseCreatedAt
    , "modified_at" .= pmOutResponseModifiedAt
    ]


instance Eq PmOutResponse where
  (==) a b = pmOutResponseId a == pmOutResponseId b && pmOutResponsePmId a == pmOutResponsePmId b && pmOutResponseUserId a == pmOutResponseUserId b && pmOutResponseLabel a == pmOutResponseLabel b && pmOutResponseIsSaved a == pmOutResponseIsSaved b && pmOutResponseActive a == pmOutResponseActive b && pmOutResponseGuard a == pmOutResponseGuard b && pmOutResponseCreatedAt a == pmOutResponseCreatedAt b && pmOutResponseModifiedAt a == pmOutResponseModifiedAt b

instance Show PmOutResponse where
    show rec = "pmOutResponseId: " <> show (pmOutResponseId rec) <> ", " <> "pmOutResponsePmId: " <> show (pmOutResponsePmId rec) <> ", " <> "pmOutResponseUserId: " <> show (pmOutResponseUserId rec) <> ", " <> "pmOutResponseLabel: " <> show (pmOutResponseLabel rec) <> ", " <> "pmOutResponseIsSaved: " <> show (pmOutResponseIsSaved rec) <> ", " <> "pmOutResponseActive: " <> show (pmOutResponseActive rec) <> ", " <> "pmOutResponseGuard: " <> show (pmOutResponseGuard rec) <> ", " <> "pmOutResponseCreatedAt: " <> show (pmOutResponseCreatedAt rec) <> ", " <> "pmOutResponseModifiedAt: " <> show (pmOutResponseModifiedAt rec)

instance FromJSON PmOutResponses where
  parseJSON (Object o) = do
    pmOutResponses <- o .: ("pm_out_responses" :: Text)
    pure $ PmOutResponses {
      pmOutResponses = pmOutResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON PmOutResponses where
  toJSON PmOutResponses{..} = object $
    [ "tag" .= ("PmOutResponses" :: Text)
    , "pm_out_responses" .= pmOutResponses
    ]


instance Eq PmOutResponses where
  (==) a b = pmOutResponses a == pmOutResponses b

instance Show PmOutResponses where
    show rec = "pmOutResponses: " <> show (pmOutResponses rec)
-- footer