{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Like where


import LN.T.Ent


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

data LikeOpt
  = Like 
  | Neutral 
  | Dislike 
  deriving (Generic,Typeable,NFData)


instance FromJSON LikeOpt where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("Like" :: Text) -> do
        pure Like

      ("Neutral" :: Text) -> do
        pure Neutral

      ("Dislike" :: Text) -> do
        pure Dislike

      _ -> fail "Could not parse LikeOpt"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LikeOpt where
  toJSON (Like ) = object $
    [ "tag" .= ("Like" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Neutral ) = object $
    [ "tag" .= ("Neutral" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Dislike ) = object $
    [ "tag" .= ("Dislike" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq LikeOpt where
  (==) Like Like = True
  (==) Neutral Neutral = True
  (==) Dislike Dislike = True
  (==) _ _ = False

instance Show LikeOpt where
  show Like = "like"
  show Neutral = "neutral"
  show Dislike = "dislike"


instance Read LikeOpt where
  readsPrec _ "like" = [(Like, "")]
  readsPrec _ "neutral" = [(Neutral, "")]
  readsPrec _ "dislike" = [(Dislike, "")]
  readsPrec _ _ = []


data LikeRequest = LikeRequest {
  likeRequestOpt :: !(LikeOpt),
  likeRequestReason :: !((Maybe Text)),
  likeRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON LikeRequest where
  parseJSON (Object o) = do
    likeRequestOpt <- o .: ("opt" :: Text)
    likeRequestReason <- o .: ("reason" :: Text)
    likeRequestGuard <- o .: ("guard" :: Text)
    pure $ LikeRequest {
      likeRequestOpt = likeRequestOpt,
      likeRequestReason = likeRequestReason,
      likeRequestGuard = likeRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LikeRequest where
  toJSON LikeRequest{..} = object $
    [ "tag" .= ("LikeRequest" :: Text)
    , "opt" .= likeRequestOpt
    , "reason" .= likeRequestReason
    , "guard" .= likeRequestGuard
    ]


instance Eq LikeRequest where
  (==) a b = likeRequestOpt a == likeRequestOpt b && likeRequestReason a == likeRequestReason b && likeRequestGuard a == likeRequestGuard b

instance Show LikeRequest where
    show rec = "likeRequestOpt: " <> show (likeRequestOpt rec) <> ", " <> "likeRequestReason: " <> show (likeRequestReason rec) <> ", " <> "likeRequestGuard: " <> show (likeRequestGuard rec)

data LikeResponse = LikeResponse {
  likeResponseId :: !(Int64),
  likeResponseEnt :: !(Ent),
  likeResponseEntId :: !(Int64),
  likeResponseUserId :: !(Int64),
  likeResponseOpt :: !(LikeOpt),
  likeResponseScore :: !(Int),
  likeResponseReason :: !((Maybe Text)),
  likeResponseActive :: !(Bool),
  likeResponseGuard :: !(Int),
  likeResponseCreatedAt :: !((Maybe UTCTime)),
  likeResponseModifiedAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON LikeResponse where
  parseJSON (Object o) = do
    likeResponseId <- o .: ("id" :: Text)
    likeResponseEnt <- o .: ("ent" :: Text)
    likeResponseEntId <- o .: ("ent_id" :: Text)
    likeResponseUserId <- o .: ("user_id" :: Text)
    likeResponseOpt <- o .: ("opt" :: Text)
    likeResponseScore <- o .: ("score" :: Text)
    likeResponseReason <- o .: ("reason" :: Text)
    likeResponseActive <- o .: ("active" :: Text)
    likeResponseGuard <- o .: ("guard" :: Text)
    likeResponseCreatedAt <- o .: ("created_at" :: Text)
    likeResponseModifiedAt <- o .: ("modified_at" :: Text)
    pure $ LikeResponse {
      likeResponseId = likeResponseId,
      likeResponseEnt = likeResponseEnt,
      likeResponseEntId = likeResponseEntId,
      likeResponseUserId = likeResponseUserId,
      likeResponseOpt = likeResponseOpt,
      likeResponseScore = likeResponseScore,
      likeResponseReason = likeResponseReason,
      likeResponseActive = likeResponseActive,
      likeResponseGuard = likeResponseGuard,
      likeResponseCreatedAt = likeResponseCreatedAt,
      likeResponseModifiedAt = likeResponseModifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LikeResponse where
  toJSON LikeResponse{..} = object $
    [ "tag" .= ("LikeResponse" :: Text)
    , "id" .= likeResponseId
    , "ent" .= likeResponseEnt
    , "ent_id" .= likeResponseEntId
    , "user_id" .= likeResponseUserId
    , "opt" .= likeResponseOpt
    , "score" .= likeResponseScore
    , "reason" .= likeResponseReason
    , "active" .= likeResponseActive
    , "guard" .= likeResponseGuard
    , "created_at" .= likeResponseCreatedAt
    , "modified_at" .= likeResponseModifiedAt
    ]


instance Eq LikeResponse where
  (==) a b = likeResponseId a == likeResponseId b && likeResponseEnt a == likeResponseEnt b && likeResponseEntId a == likeResponseEntId b && likeResponseUserId a == likeResponseUserId b && likeResponseOpt a == likeResponseOpt b && likeResponseScore a == likeResponseScore b && likeResponseReason a == likeResponseReason b && likeResponseActive a == likeResponseActive b && likeResponseGuard a == likeResponseGuard b && likeResponseCreatedAt a == likeResponseCreatedAt b && likeResponseModifiedAt a == likeResponseModifiedAt b

instance Show LikeResponse where
    show rec = "likeResponseId: " <> show (likeResponseId rec) <> ", " <> "likeResponseEnt: " <> show (likeResponseEnt rec) <> ", " <> "likeResponseEntId: " <> show (likeResponseEntId rec) <> ", " <> "likeResponseUserId: " <> show (likeResponseUserId rec) <> ", " <> "likeResponseOpt: " <> show (likeResponseOpt rec) <> ", " <> "likeResponseScore: " <> show (likeResponseScore rec) <> ", " <> "likeResponseReason: " <> show (likeResponseReason rec) <> ", " <> "likeResponseActive: " <> show (likeResponseActive rec) <> ", " <> "likeResponseGuard: " <> show (likeResponseGuard rec) <> ", " <> "likeResponseCreatedAt: " <> show (likeResponseCreatedAt rec) <> ", " <> "likeResponseModifiedAt: " <> show (likeResponseModifiedAt rec)

data LikeResponses = LikeResponses {
  likeResponses :: !([LikeResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON LikeResponses where
  parseJSON (Object o) = do
    likeResponses <- o .: ("like_responses" :: Text)
    pure $ LikeResponses {
      likeResponses = likeResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LikeResponses where
  toJSON LikeResponses{..} = object $
    [ "tag" .= ("LikeResponses" :: Text)
    , "like_responses" .= likeResponses
    ]


instance Eq LikeResponses where
  (==) a b = likeResponses a == likeResponses b

instance Show LikeResponses where
    show rec = "likeResponses: " <> show (likeResponses rec)

data LikeStatResponse = LikeStatResponse {
  likeStatResponseEnt :: !(Ent),
  likeStatResponseEntId :: !(Int64),
  likeStatResponseScore :: !(Int64),
  likeStatResponseLike :: !(Int64),
  likeStatResponseNeutral :: !(Int64),
  likeStatResponseDislike :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON LikeStatResponse where
  parseJSON (Object o) = do
    likeStatResponseEnt <- o .: ("ent" :: Text)
    likeStatResponseEntId <- o .: ("ent_id" :: Text)
    likeStatResponseScore <- o .: ("score" :: Text)
    likeStatResponseLike <- o .: ("like" :: Text)
    likeStatResponseNeutral <- o .: ("neutral" :: Text)
    likeStatResponseDislike <- o .: ("dislike" :: Text)
    pure $ LikeStatResponse {
      likeStatResponseEnt = likeStatResponseEnt,
      likeStatResponseEntId = likeStatResponseEntId,
      likeStatResponseScore = likeStatResponseScore,
      likeStatResponseLike = likeStatResponseLike,
      likeStatResponseNeutral = likeStatResponseNeutral,
      likeStatResponseDislike = likeStatResponseDislike
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LikeStatResponse where
  toJSON LikeStatResponse{..} = object $
    [ "tag" .= ("LikeStatResponse" :: Text)
    , "ent" .= likeStatResponseEnt
    , "ent_id" .= likeStatResponseEntId
    , "score" .= likeStatResponseScore
    , "like" .= likeStatResponseLike
    , "neutral" .= likeStatResponseNeutral
    , "dislike" .= likeStatResponseDislike
    ]


instance Eq LikeStatResponse where
  (==) a b = likeStatResponseEnt a == likeStatResponseEnt b && likeStatResponseEntId a == likeStatResponseEntId b && likeStatResponseScore a == likeStatResponseScore b && likeStatResponseLike a == likeStatResponseLike b && likeStatResponseNeutral a == likeStatResponseNeutral b && likeStatResponseDislike a == likeStatResponseDislike b

instance Show LikeStatResponse where
    show rec = "likeStatResponseEnt: " <> show (likeStatResponseEnt rec) <> ", " <> "likeStatResponseEntId: " <> show (likeStatResponseEntId rec) <> ", " <> "likeStatResponseScore: " <> show (likeStatResponseScore rec) <> ", " <> "likeStatResponseLike: " <> show (likeStatResponseLike rec) <> ", " <> "likeStatResponseNeutral: " <> show (likeStatResponseNeutral rec) <> ", " <> "likeStatResponseDislike: " <> show (likeStatResponseDislike rec)

data LikeStatResponses = LikeStatResponses {
  likeStatResponses :: !([LikeStatResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON LikeStatResponses where
  parseJSON (Object o) = do
    likeStatResponses <- o .: ("like_stat_responses" :: Text)
    pure $ LikeStatResponses {
      likeStatResponses = likeStatResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LikeStatResponses where
  toJSON LikeStatResponses{..} = object $
    [ "tag" .= ("LikeStatResponses" :: Text)
    , "like_stat_responses" .= likeStatResponses
    ]


instance Eq LikeStatResponses where
  (==) a b = likeStatResponses a == likeStatResponses b

instance Show LikeStatResponses where
    show rec = "likeStatResponses: " <> show (likeStatResponses rec)
-- footer