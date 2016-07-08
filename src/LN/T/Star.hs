{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Star where


import LN.T.Ent


import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Int            (Int64)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)
import           Data.Default

data StarRequest = StarRequest {
  starRequestReason :: (Maybe Text),
  starRequestGuard :: Int
}


instance FromJSON StarRequest where
  parseJSON (Object o) = do
    starRequestReason <- o .: ("reason" :: Text)
    starRequestGuard <- o .: ("guard" :: Text)
    pure $ StarRequest {
      starRequestReason = starRequestReason,
      starRequestGuard = starRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON StarRequest where
  toJSON StarRequest{..} = object $
    [ "tag" .= ("StarRequest" :: Text)
    , "reason" .= starRequestReason
    , "guard" .= starRequestGuard
    ]


instance Eq StarRequest where
  (==) a b = starRequestReason a == starRequestReason b && starRequestGuard a == starRequestGuard b

instance Show StarRequest where
    show rec = "starRequestReason: " <> show (starRequestReason rec) <> ", " <> "starRequestGuard: " <> show (starRequestGuard rec)

data StarResponse = StarResponse {
  starResponseId :: Int64,
  starResponseEnt :: Ent,
  starResponseEntId :: Int64,
  starResponseUserId :: Int64,
  starResponseReason :: (Maybe Text),
  starResponseActive :: Bool,
  starResponseGuard :: Int,
  starResponseCreatedAt :: (Maybe UTCTime),
  starResponseModifiedAt :: (Maybe UTCTime)
}


instance FromJSON StarResponse where
  parseJSON (Object o) = do
    starResponseId <- o .: ("id" :: Text)
    starResponseEnt <- o .: ("ent" :: Text)
    starResponseEntId <- o .: ("ent_id" :: Text)
    starResponseUserId <- o .: ("user_id" :: Text)
    starResponseReason <- o .: ("reason" :: Text)
    starResponseActive <- o .: ("active" :: Text)
    starResponseGuard <- o .: ("guard" :: Text)
    starResponseCreatedAt <- o .: ("created_at" :: Text)
    starResponseModifiedAt <- o .: ("modified_at" :: Text)
    pure $ StarResponse {
      starResponseId = starResponseId,
      starResponseEnt = starResponseEnt,
      starResponseEntId = starResponseEntId,
      starResponseUserId = starResponseUserId,
      starResponseReason = starResponseReason,
      starResponseActive = starResponseActive,
      starResponseGuard = starResponseGuard,
      starResponseCreatedAt = starResponseCreatedAt,
      starResponseModifiedAt = starResponseModifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON StarResponse where
  toJSON StarResponse{..} = object $
    [ "tag" .= ("StarResponse" :: Text)
    , "id" .= starResponseId
    , "ent" .= starResponseEnt
    , "ent_id" .= starResponseEntId
    , "user_id" .= starResponseUserId
    , "reason" .= starResponseReason
    , "active" .= starResponseActive
    , "guard" .= starResponseGuard
    , "created_at" .= starResponseCreatedAt
    , "modified_at" .= starResponseModifiedAt
    ]


instance Eq StarResponse where
  (==) a b = starResponseId a == starResponseId b && starResponseEnt a == starResponseEnt b && starResponseEntId a == starResponseEntId b && starResponseUserId a == starResponseUserId b && starResponseReason a == starResponseReason b && starResponseActive a == starResponseActive b && starResponseGuard a == starResponseGuard b && starResponseCreatedAt a == starResponseCreatedAt b && starResponseModifiedAt a == starResponseModifiedAt b

instance Show StarResponse where
    show rec = "starResponseId: " <> show (starResponseId rec) <> ", " <> "starResponseEnt: " <> show (starResponseEnt rec) <> ", " <> "starResponseEntId: " <> show (starResponseEntId rec) <> ", " <> "starResponseUserId: " <> show (starResponseUserId rec) <> ", " <> "starResponseReason: " <> show (starResponseReason rec) <> ", " <> "starResponseActive: " <> show (starResponseActive rec) <> ", " <> "starResponseGuard: " <> show (starResponseGuard rec) <> ", " <> "starResponseCreatedAt: " <> show (starResponseCreatedAt rec) <> ", " <> "starResponseModifiedAt: " <> show (starResponseModifiedAt rec)

data StarResponses = StarResponses {
  starResponses :: [StarResponse]
}


instance FromJSON StarResponses where
  parseJSON (Object o) = do
    starResponses <- o .: ("star_responses" :: Text)
    pure $ StarResponses {
      starResponses = starResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON StarResponses where
  toJSON StarResponses{..} = object $
    [ "tag" .= ("StarResponses" :: Text)
    , "star_responses" .= starResponses
    ]


instance Eq StarResponses where
  (==) a b = starResponses a == starResponses b

instance Show StarResponses where
    show rec = "starResponses: " <> show (starResponses rec)

data StarStatResponse = StarStatResponse {
  starStatResponseEnt :: Ent,
  starStatResponseEntId :: Int64,
  starStatResponseStars :: Int64
}


instance FromJSON StarStatResponse where
  parseJSON (Object o) = do
    starStatResponseEnt <- o .: ("ent" :: Text)
    starStatResponseEntId <- o .: ("ent_id" :: Text)
    starStatResponseStars <- o .: ("stars" :: Text)
    pure $ StarStatResponse {
      starStatResponseEnt = starStatResponseEnt,
      starStatResponseEntId = starStatResponseEntId,
      starStatResponseStars = starStatResponseStars
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON StarStatResponse where
  toJSON StarStatResponse{..} = object $
    [ "tag" .= ("StarStatResponse" :: Text)
    , "ent" .= starStatResponseEnt
    , "ent_id" .= starStatResponseEntId
    , "stars" .= starStatResponseStars
    ]


instance Eq StarStatResponse where
  (==) a b = starStatResponseEnt a == starStatResponseEnt b && starStatResponseEntId a == starStatResponseEntId b && starStatResponseStars a == starStatResponseStars b

instance Show StarStatResponse where
    show rec = "starStatResponseEnt: " <> show (starStatResponseEnt rec) <> ", " <> "starStatResponseEntId: " <> show (starStatResponseEntId rec) <> ", " <> "starStatResponseStars: " <> show (starStatResponseStars rec)

data StarStatResponses = StarStatResponses {
  starStatResponses :: [StarStatResponse]
}


instance FromJSON StarStatResponses where
  parseJSON (Object o) = do
    starStatResponses <- o .: ("star_stat_responses" :: Text)
    pure $ StarStatResponses {
      starStatResponses = starStatResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON StarStatResponses where
  toJSON StarStatResponses{..} = object $
    [ "tag" .= ("StarStatResponses" :: Text)
    , "star_stat_responses" .= starStatResponses
    ]


instance Eq StarStatResponses where
  (==) a b = starStatResponses a == starStatResponses b

instance Show StarStatResponses where
    show rec = "starStatResponses: " <> show (starStatResponses rec)
-- footer