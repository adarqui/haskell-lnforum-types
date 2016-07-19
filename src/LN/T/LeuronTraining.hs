{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.LeuronTraining where





import           Control.DeepSeq     (NFData)
import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Default
import           Data.Int            (Int64)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Typeable       (Typeable)
import           Data.Monoid         ((<>))
import           GHC.Generics        (Generic)
import           Haskell.Api.Helpers (QueryParam, qp)

data LeuronTrainingSummary
  = LTS_View 
  | LTS_Skip 
  | LTS_Know 
  | LTS_DontKnow 
  | LTS_DontUnderstand 
  | LTS_DontCare 
  | LTS_Protest 
  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronTrainingSummary where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("LTS_View" :: Text) -> do
        pure LTS_View

      ("LTS_Skip" :: Text) -> do
        pure LTS_Skip

      ("LTS_Know" :: Text) -> do
        pure LTS_Know

      ("LTS_DontKnow" :: Text) -> do
        pure LTS_DontKnow

      ("LTS_DontUnderstand" :: Text) -> do
        pure LTS_DontUnderstand

      ("LTS_DontCare" :: Text) -> do
        pure LTS_DontCare

      ("LTS_Protest" :: Text) -> do
        pure LTS_Protest

      _ -> fail "Could not parse LeuronTrainingSummary"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronTrainingSummary where
  toJSON (LTS_View ) = object $
    [ "tag" .= ("LTS_View" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (LTS_Skip ) = object $
    [ "tag" .= ("LTS_Skip" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (LTS_Know ) = object $
    [ "tag" .= ("LTS_Know" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (LTS_DontKnow ) = object $
    [ "tag" .= ("LTS_DontKnow" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (LTS_DontUnderstand ) = object $
    [ "tag" .= ("LTS_DontUnderstand" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (LTS_DontCare ) = object $
    [ "tag" .= ("LTS_DontCare" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (LTS_Protest ) = object $
    [ "tag" .= ("LTS_Protest" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq LeuronTrainingSummary where
  (==) LTS_View LTS_View = True
  (==) LTS_Skip LTS_Skip = True
  (==) LTS_Know LTS_Know = True
  (==) LTS_DontKnow LTS_DontKnow = True
  (==) LTS_DontUnderstand LTS_DontUnderstand = True
  (==) LTS_DontCare LTS_DontCare = True
  (==) LTS_Protest LTS_Protest = True
  (==) _ _ = False

instance Show LeuronTrainingSummary where
  show LTS_View = "lts_view"
  show LTS_Skip = "lts_skip"
  show LTS_Know = "lts_know"
  show LTS_DontKnow = "lts_dont_know"
  show LTS_DontUnderstand = "lts_dont_understand"
  show LTS_DontCare = "lts_dont_care"
  show LTS_Protest = "lts_protest"


instance Read LeuronTrainingSummary where
  readsPrec _ "lts_view" = [(LTS_View, "")]
  readsPrec _ "lts_skip" = [(LTS_Skip, "")]
  readsPrec _ "lts_know" = [(LTS_Know, "")]
  readsPrec _ "lts_dont_know" = [(LTS_DontKnow, "")]
  readsPrec _ "lts_dont_understand" = [(LTS_DontUnderstand, "")]
  readsPrec _ "lts_dont_care" = [(LTS_DontCare, "")]
  readsPrec _ "lts_protest" = [(LTS_Protest, "")]
  readsPrec _ _ = []


data LeuronTrainingRequest = LeuronTrainingRequest {
  leuronTrainingRequestSummary :: !(LeuronTrainingSummary),
  leuronTrainingRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronTrainingRequest where
  parseJSON (Object o) = do
    leuronTrainingRequestSummary <- o .: ("summary" :: Text)
    leuronTrainingRequestGuard <- o .: ("guard" :: Text)
    pure $ LeuronTrainingRequest {
      leuronTrainingRequestSummary = leuronTrainingRequestSummary,
      leuronTrainingRequestGuard = leuronTrainingRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronTrainingRequest where
  toJSON LeuronTrainingRequest{..} = object $
    [ "tag" .= ("LeuronTrainingRequest" :: Text)
    , "summary" .= leuronTrainingRequestSummary
    , "guard" .= leuronTrainingRequestGuard
    ]


instance Eq LeuronTrainingRequest where
  (==) a b = leuronTrainingRequestSummary a == leuronTrainingRequestSummary b && leuronTrainingRequestGuard a == leuronTrainingRequestGuard b

instance Show LeuronTrainingRequest where
    show rec = "leuronTrainingRequestSummary: " <> show (leuronTrainingRequestSummary rec) <> ", " <> "leuronTrainingRequestGuard: " <> show (leuronTrainingRequestGuard rec)

data LeuronTrainingResponse = LeuronTrainingResponse {
  leuronTrainingResponseId :: !(Int64),
  leuronTrainingResponseUserId :: !(Int64),
  leuronTrainingResponseLeuronId :: !(Int64),
  leuronTrainingResponseSummary :: !(LeuronTrainingSummary),
  leuronTrainingResponseGuard :: !(Int),
  leuronTrainingResponseCreatedAt :: !((Maybe UTCTime)),
  leuronTrainingResponseModifiedAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronTrainingResponse where
  parseJSON (Object o) = do
    leuronTrainingResponseId <- o .: ("id" :: Text)
    leuronTrainingResponseUserId <- o .: ("user_id" :: Text)
    leuronTrainingResponseLeuronId <- o .: ("leuron_id" :: Text)
    leuronTrainingResponseSummary <- o .: ("summary" :: Text)
    leuronTrainingResponseGuard <- o .: ("guard" :: Text)
    leuronTrainingResponseCreatedAt <- o .: ("created_at" :: Text)
    leuronTrainingResponseModifiedAt <- o .: ("modified_at" :: Text)
    pure $ LeuronTrainingResponse {
      leuronTrainingResponseId = leuronTrainingResponseId,
      leuronTrainingResponseUserId = leuronTrainingResponseUserId,
      leuronTrainingResponseLeuronId = leuronTrainingResponseLeuronId,
      leuronTrainingResponseSummary = leuronTrainingResponseSummary,
      leuronTrainingResponseGuard = leuronTrainingResponseGuard,
      leuronTrainingResponseCreatedAt = leuronTrainingResponseCreatedAt,
      leuronTrainingResponseModifiedAt = leuronTrainingResponseModifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronTrainingResponse where
  toJSON LeuronTrainingResponse{..} = object $
    [ "tag" .= ("LeuronTrainingResponse" :: Text)
    , "id" .= leuronTrainingResponseId
    , "user_id" .= leuronTrainingResponseUserId
    , "leuron_id" .= leuronTrainingResponseLeuronId
    , "summary" .= leuronTrainingResponseSummary
    , "guard" .= leuronTrainingResponseGuard
    , "created_at" .= leuronTrainingResponseCreatedAt
    , "modified_at" .= leuronTrainingResponseModifiedAt
    ]


instance Eq LeuronTrainingResponse where
  (==) a b = leuronTrainingResponseId a == leuronTrainingResponseId b && leuronTrainingResponseUserId a == leuronTrainingResponseUserId b && leuronTrainingResponseLeuronId a == leuronTrainingResponseLeuronId b && leuronTrainingResponseSummary a == leuronTrainingResponseSummary b && leuronTrainingResponseGuard a == leuronTrainingResponseGuard b && leuronTrainingResponseCreatedAt a == leuronTrainingResponseCreatedAt b && leuronTrainingResponseModifiedAt a == leuronTrainingResponseModifiedAt b

instance Show LeuronTrainingResponse where
    show rec = "leuronTrainingResponseId: " <> show (leuronTrainingResponseId rec) <> ", " <> "leuronTrainingResponseUserId: " <> show (leuronTrainingResponseUserId rec) <> ", " <> "leuronTrainingResponseLeuronId: " <> show (leuronTrainingResponseLeuronId rec) <> ", " <> "leuronTrainingResponseSummary: " <> show (leuronTrainingResponseSummary rec) <> ", " <> "leuronTrainingResponseGuard: " <> show (leuronTrainingResponseGuard rec) <> ", " <> "leuronTrainingResponseCreatedAt: " <> show (leuronTrainingResponseCreatedAt rec) <> ", " <> "leuronTrainingResponseModifiedAt: " <> show (leuronTrainingResponseModifiedAt rec)

data LeuronTrainingResponses = LeuronTrainingResponses {
  leuronTrainingResponses :: !([LeuronTrainingResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronTrainingResponses where
  parseJSON (Object o) = do
    leuronTrainingResponses <- o .: ("leuron_training_responses" :: Text)
    pure $ LeuronTrainingResponses {
      leuronTrainingResponses = leuronTrainingResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronTrainingResponses where
  toJSON LeuronTrainingResponses{..} = object $
    [ "tag" .= ("LeuronTrainingResponses" :: Text)
    , "leuron_training_responses" .= leuronTrainingResponses
    ]


instance Eq LeuronTrainingResponses where
  (==) a b = leuronTrainingResponses a == leuronTrainingResponses b

instance Show LeuronTrainingResponses where
    show rec = "leuronTrainingResponses: " <> show (leuronTrainingResponses rec)

data LeuronTrainingStatResponse = LeuronTrainingStatResponse {
  leuronTrainingStatResponseLeuronTrainingId :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronTrainingStatResponse where
  parseJSON (Object o) = do
    leuronTrainingStatResponseLeuronTrainingId <- o .: ("leuron_training_id" :: Text)
    pure $ LeuronTrainingStatResponse {
      leuronTrainingStatResponseLeuronTrainingId = leuronTrainingStatResponseLeuronTrainingId
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronTrainingStatResponse where
  toJSON LeuronTrainingStatResponse{..} = object $
    [ "tag" .= ("LeuronTrainingStatResponse" :: Text)
    , "leuron_training_id" .= leuronTrainingStatResponseLeuronTrainingId
    ]


instance Eq LeuronTrainingStatResponse where
  (==) a b = leuronTrainingStatResponseLeuronTrainingId a == leuronTrainingStatResponseLeuronTrainingId b

instance Show LeuronTrainingStatResponse where
    show rec = "leuronTrainingStatResponseLeuronTrainingId: " <> show (leuronTrainingStatResponseLeuronTrainingId rec)

data LeuronTrainingStatResponses = LeuronTrainingStatResponses {
  leuronTrainingStatResponses :: !([LeuronTrainingStatResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronTrainingStatResponses where
  parseJSON (Object o) = do
    leuronTrainingStatResponses <- o .: ("leuron_training_stat_responses" :: Text)
    pure $ LeuronTrainingStatResponses {
      leuronTrainingStatResponses = leuronTrainingStatResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronTrainingStatResponses where
  toJSON LeuronTrainingStatResponses{..} = object $
    [ "tag" .= ("LeuronTrainingStatResponses" :: Text)
    , "leuron_training_stat_responses" .= leuronTrainingStatResponses
    ]


instance Eq LeuronTrainingStatResponses where
  (==) a b = leuronTrainingStatResponses a == leuronTrainingStatResponses b

instance Show LeuronTrainingStatResponses where
    show rec = "leuronTrainingStatResponses: " <> show (leuronTrainingStatResponses rec)
-- footer