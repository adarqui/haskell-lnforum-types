{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Leuron where


import LN.T.DepList
import LN.T.Splits
import LN.T.Substitutions


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

data LeuronRequest = LeuronRequest {
  leuronRequestData :: !(LeuronData),
  leuronRequestTitle :: !((Maybe Text)),
  leuronRequestDescription :: !((Maybe Text)),
  leuronRequestSection :: !((Maybe Text)),
  leuronRequestPage :: !((Maybe Text)),
  leuronRequestExamples :: !((Maybe [Text])),
  leuronRequestStrengths :: !((Maybe [Text])),
  leuronRequestCategories :: !((DepList Text)),
  leuronRequestSplits :: !((Maybe [Splits])),
  leuronRequestSubstitutions :: !((Maybe [Substitutions])),
  leuronRequestTags :: !([Text]),
  leuronRequestStyle :: !((Maybe [Text])),
  leuronRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronRequest where
  parseJSON (Object o) = do
    leuronRequestData <- o .: ("data" :: Text)
    leuronRequestTitle <- o .: ("title" :: Text)
    leuronRequestDescription <- o .: ("description" :: Text)
    leuronRequestSection <- o .: ("section" :: Text)
    leuronRequestPage <- o .: ("page" :: Text)
    leuronRequestExamples <- o .: ("examples" :: Text)
    leuronRequestStrengths <- o .: ("strengths" :: Text)
    leuronRequestCategories <- o .: ("categories" :: Text)
    leuronRequestSplits <- o .: ("splits" :: Text)
    leuronRequestSubstitutions <- o .: ("substitutions" :: Text)
    leuronRequestTags <- o .: ("tags" :: Text)
    leuronRequestStyle <- o .: ("style" :: Text)
    leuronRequestGuard <- o .: ("guard" :: Text)
    pure $ LeuronRequest {
      leuronRequestData = leuronRequestData,
      leuronRequestTitle = leuronRequestTitle,
      leuronRequestDescription = leuronRequestDescription,
      leuronRequestSection = leuronRequestSection,
      leuronRequestPage = leuronRequestPage,
      leuronRequestExamples = leuronRequestExamples,
      leuronRequestStrengths = leuronRequestStrengths,
      leuronRequestCategories = leuronRequestCategories,
      leuronRequestSplits = leuronRequestSplits,
      leuronRequestSubstitutions = leuronRequestSubstitutions,
      leuronRequestTags = leuronRequestTags,
      leuronRequestStyle = leuronRequestStyle,
      leuronRequestGuard = leuronRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronRequest where
  toJSON LeuronRequest{..} = object $
    [ "tag" .= ("LeuronRequest" :: Text)
    , "data" .= leuronRequestData
    , "title" .= leuronRequestTitle
    , "description" .= leuronRequestDescription
    , "section" .= leuronRequestSection
    , "page" .= leuronRequestPage
    , "examples" .= leuronRequestExamples
    , "strengths" .= leuronRequestStrengths
    , "categories" .= leuronRequestCategories
    , "splits" .= leuronRequestSplits
    , "substitutions" .= leuronRequestSubstitutions
    , "tags" .= leuronRequestTags
    , "style" .= leuronRequestStyle
    , "guard" .= leuronRequestGuard
    ]


instance Eq LeuronRequest where
  (==) a b = leuronRequestData a == leuronRequestData b && leuronRequestTitle a == leuronRequestTitle b && leuronRequestDescription a == leuronRequestDescription b && leuronRequestSection a == leuronRequestSection b && leuronRequestPage a == leuronRequestPage b && leuronRequestExamples a == leuronRequestExamples b && leuronRequestStrengths a == leuronRequestStrengths b && leuronRequestCategories a == leuronRequestCategories b && leuronRequestSplits a == leuronRequestSplits b && leuronRequestSubstitutions a == leuronRequestSubstitutions b && leuronRequestTags a == leuronRequestTags b && leuronRequestStyle a == leuronRequestStyle b && leuronRequestGuard a == leuronRequestGuard b

instance Show LeuronRequest where
    show rec = "leuronRequestData: " <> show (leuronRequestData rec) <> ", " <> "leuronRequestTitle: " <> show (leuronRequestTitle rec) <> ", " <> "leuronRequestDescription: " <> show (leuronRequestDescription rec) <> ", " <> "leuronRequestSection: " <> show (leuronRequestSection rec) <> ", " <> "leuronRequestPage: " <> show (leuronRequestPage rec) <> ", " <> "leuronRequestExamples: " <> show (leuronRequestExamples rec) <> ", " <> "leuronRequestStrengths: " <> show (leuronRequestStrengths rec) <> ", " <> "leuronRequestCategories: " <> show (leuronRequestCategories rec) <> ", " <> "leuronRequestSplits: " <> show (leuronRequestSplits rec) <> ", " <> "leuronRequestSubstitutions: " <> show (leuronRequestSubstitutions rec) <> ", " <> "leuronRequestTags: " <> show (leuronRequestTags rec) <> ", " <> "leuronRequestStyle: " <> show (leuronRequestStyle rec) <> ", " <> "leuronRequestGuard: " <> show (leuronRequestGuard rec)

data LeuronResponse = LeuronResponse {
  leuronResponseId :: !(Int64),
  leuronResponseUserId :: !(Int64),
  leuronResponseResourceId :: !(Int64),
  leuronResponseData :: !(LeuronData),
  leuronResponseTitle :: !((Maybe Text)),
  leuronResponseDescription :: !((Maybe Text)),
  leuronResponseSection :: !((Maybe Text)),
  leuronResponsePage :: !((Maybe Text)),
  leuronResponseExamples :: !((Maybe [Text])),
  leuronResponseStrengths :: !((Maybe [Text])),
  leuronResponseCategories :: !((DepList Text)),
  leuronResponseSplits :: !((Maybe [Splits])),
  leuronResponseSubstitutions :: !((Maybe [Substitutions])),
  leuronResponseTags :: !([Text]),
  leuronResponseStyle :: !((Maybe [Text])),
  leuronResponseActive :: !(Bool),
  leuronResponseGuard :: !(Int),
  leuronResponseCreatedAt :: !((Maybe UTCTime)),
  leuronResponseModifiedAt :: !((Maybe UTCTime)),
  leuronResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronResponse where
  parseJSON (Object o) = do
    leuronResponseId <- o .: ("id" :: Text)
    leuronResponseUserId <- o .: ("user_id" :: Text)
    leuronResponseResourceId <- o .: ("resource_id" :: Text)
    leuronResponseData <- o .: ("data" :: Text)
    leuronResponseTitle <- o .: ("title" :: Text)
    leuronResponseDescription <- o .: ("description" :: Text)
    leuronResponseSection <- o .: ("section" :: Text)
    leuronResponsePage <- o .: ("page" :: Text)
    leuronResponseExamples <- o .: ("examples" :: Text)
    leuronResponseStrengths <- o .: ("strengths" :: Text)
    leuronResponseCategories <- o .: ("categories" :: Text)
    leuronResponseSplits <- o .: ("splits" :: Text)
    leuronResponseSubstitutions <- o .: ("substitutions" :: Text)
    leuronResponseTags <- o .: ("tags" :: Text)
    leuronResponseStyle <- o .: ("style" :: Text)
    leuronResponseActive <- o .: ("active" :: Text)
    leuronResponseGuard <- o .: ("guard" :: Text)
    leuronResponseCreatedAt <- o .: ("created_at" :: Text)
    leuronResponseModifiedAt <- o .: ("modified_at" :: Text)
    leuronResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ LeuronResponse {
      leuronResponseId = leuronResponseId,
      leuronResponseUserId = leuronResponseUserId,
      leuronResponseResourceId = leuronResponseResourceId,
      leuronResponseData = leuronResponseData,
      leuronResponseTitle = leuronResponseTitle,
      leuronResponseDescription = leuronResponseDescription,
      leuronResponseSection = leuronResponseSection,
      leuronResponsePage = leuronResponsePage,
      leuronResponseExamples = leuronResponseExamples,
      leuronResponseStrengths = leuronResponseStrengths,
      leuronResponseCategories = leuronResponseCategories,
      leuronResponseSplits = leuronResponseSplits,
      leuronResponseSubstitutions = leuronResponseSubstitutions,
      leuronResponseTags = leuronResponseTags,
      leuronResponseStyle = leuronResponseStyle,
      leuronResponseActive = leuronResponseActive,
      leuronResponseGuard = leuronResponseGuard,
      leuronResponseCreatedAt = leuronResponseCreatedAt,
      leuronResponseModifiedAt = leuronResponseModifiedAt,
      leuronResponseActivityAt = leuronResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronResponse where
  toJSON LeuronResponse{..} = object $
    [ "tag" .= ("LeuronResponse" :: Text)
    , "id" .= leuronResponseId
    , "user_id" .= leuronResponseUserId
    , "resource_id" .= leuronResponseResourceId
    , "data" .= leuronResponseData
    , "title" .= leuronResponseTitle
    , "description" .= leuronResponseDescription
    , "section" .= leuronResponseSection
    , "page" .= leuronResponsePage
    , "examples" .= leuronResponseExamples
    , "strengths" .= leuronResponseStrengths
    , "categories" .= leuronResponseCategories
    , "splits" .= leuronResponseSplits
    , "substitutions" .= leuronResponseSubstitutions
    , "tags" .= leuronResponseTags
    , "style" .= leuronResponseStyle
    , "active" .= leuronResponseActive
    , "guard" .= leuronResponseGuard
    , "created_at" .= leuronResponseCreatedAt
    , "modified_at" .= leuronResponseModifiedAt
    , "activity_at" .= leuronResponseActivityAt
    ]


instance Eq LeuronResponse where
  (==) a b = leuronResponseId a == leuronResponseId b && leuronResponseUserId a == leuronResponseUserId b && leuronResponseResourceId a == leuronResponseResourceId b && leuronResponseData a == leuronResponseData b && leuronResponseTitle a == leuronResponseTitle b && leuronResponseDescription a == leuronResponseDescription b && leuronResponseSection a == leuronResponseSection b && leuronResponsePage a == leuronResponsePage b && leuronResponseExamples a == leuronResponseExamples b && leuronResponseStrengths a == leuronResponseStrengths b && leuronResponseCategories a == leuronResponseCategories b && leuronResponseSplits a == leuronResponseSplits b && leuronResponseSubstitutions a == leuronResponseSubstitutions b && leuronResponseTags a == leuronResponseTags b && leuronResponseStyle a == leuronResponseStyle b && leuronResponseActive a == leuronResponseActive b && leuronResponseGuard a == leuronResponseGuard b && leuronResponseCreatedAt a == leuronResponseCreatedAt b && leuronResponseModifiedAt a == leuronResponseModifiedAt b && leuronResponseActivityAt a == leuronResponseActivityAt b

instance Show LeuronResponse where
    show rec = "leuronResponseId: " <> show (leuronResponseId rec) <> ", " <> "leuronResponseUserId: " <> show (leuronResponseUserId rec) <> ", " <> "leuronResponseResourceId: " <> show (leuronResponseResourceId rec) <> ", " <> "leuronResponseData: " <> show (leuronResponseData rec) <> ", " <> "leuronResponseTitle: " <> show (leuronResponseTitle rec) <> ", " <> "leuronResponseDescription: " <> show (leuronResponseDescription rec) <> ", " <> "leuronResponseSection: " <> show (leuronResponseSection rec) <> ", " <> "leuronResponsePage: " <> show (leuronResponsePage rec) <> ", " <> "leuronResponseExamples: " <> show (leuronResponseExamples rec) <> ", " <> "leuronResponseStrengths: " <> show (leuronResponseStrengths rec) <> ", " <> "leuronResponseCategories: " <> show (leuronResponseCategories rec) <> ", " <> "leuronResponseSplits: " <> show (leuronResponseSplits rec) <> ", " <> "leuronResponseSubstitutions: " <> show (leuronResponseSubstitutions rec) <> ", " <> "leuronResponseTags: " <> show (leuronResponseTags rec) <> ", " <> "leuronResponseStyle: " <> show (leuronResponseStyle rec) <> ", " <> "leuronResponseActive: " <> show (leuronResponseActive rec) <> ", " <> "leuronResponseGuard: " <> show (leuronResponseGuard rec) <> ", " <> "leuronResponseCreatedAt: " <> show (leuronResponseCreatedAt rec) <> ", " <> "leuronResponseModifiedAt: " <> show (leuronResponseModifiedAt rec) <> ", " <> "leuronResponseActivityAt: " <> show (leuronResponseActivityAt rec)

data LeuronResponses = LeuronResponses {
  leuronResponses :: !([LeuronResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronResponses where
  parseJSON (Object o) = do
    leuronResponses <- o .: ("leuron_responses" :: Text)
    pure $ LeuronResponses {
      leuronResponses = leuronResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronResponses where
  toJSON LeuronResponses{..} = object $
    [ "tag" .= ("LeuronResponses" :: Text)
    , "leuron_responses" .= leuronResponses
    ]


instance Eq LeuronResponses where
  (==) a b = leuronResponses a == leuronResponses b

instance Show LeuronResponses where
    show rec = "leuronResponses: " <> show (leuronResponses rec)

data LeuronStatResponse = LeuronStatResponse {
  leuronStatResponseLeuronId :: !(Int64),
  leuronStatResponseLikes :: !(Int64),
  leuronStatResponseNeutral :: !(Int64),
  leuronStatResponseDislikes :: !(Int64),
  leuronStatResponseStars :: !(Int64),
  leuronStatResponseViews :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronStatResponse where
  parseJSON (Object o) = do
    leuronStatResponseLeuronId <- o .: ("leuron_id" :: Text)
    leuronStatResponseLikes <- o .: ("likes" :: Text)
    leuronStatResponseNeutral <- o .: ("neutral" :: Text)
    leuronStatResponseDislikes <- o .: ("dislikes" :: Text)
    leuronStatResponseStars <- o .: ("stars" :: Text)
    leuronStatResponseViews <- o .: ("views" :: Text)
    pure $ LeuronStatResponse {
      leuronStatResponseLeuronId = leuronStatResponseLeuronId,
      leuronStatResponseLikes = leuronStatResponseLikes,
      leuronStatResponseNeutral = leuronStatResponseNeutral,
      leuronStatResponseDislikes = leuronStatResponseDislikes,
      leuronStatResponseStars = leuronStatResponseStars,
      leuronStatResponseViews = leuronStatResponseViews
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronStatResponse where
  toJSON LeuronStatResponse{..} = object $
    [ "tag" .= ("LeuronStatResponse" :: Text)
    , "leuron_id" .= leuronStatResponseLeuronId
    , "likes" .= leuronStatResponseLikes
    , "neutral" .= leuronStatResponseNeutral
    , "dislikes" .= leuronStatResponseDislikes
    , "stars" .= leuronStatResponseStars
    , "views" .= leuronStatResponseViews
    ]


instance Eq LeuronStatResponse where
  (==) a b = leuronStatResponseLeuronId a == leuronStatResponseLeuronId b && leuronStatResponseLikes a == leuronStatResponseLikes b && leuronStatResponseNeutral a == leuronStatResponseNeutral b && leuronStatResponseDislikes a == leuronStatResponseDislikes b && leuronStatResponseStars a == leuronStatResponseStars b && leuronStatResponseViews a == leuronStatResponseViews b

instance Show LeuronStatResponse where
    show rec = "leuronStatResponseLeuronId: " <> show (leuronStatResponseLeuronId rec) <> ", " <> "leuronStatResponseLikes: " <> show (leuronStatResponseLikes rec) <> ", " <> "leuronStatResponseNeutral: " <> show (leuronStatResponseNeutral rec) <> ", " <> "leuronStatResponseDislikes: " <> show (leuronStatResponseDislikes rec) <> ", " <> "leuronStatResponseStars: " <> show (leuronStatResponseStars rec) <> ", " <> "leuronStatResponseViews: " <> show (leuronStatResponseViews rec)

data LeuronStatResponses = LeuronStatResponses {
  leuronStatResponses :: !([LeuronStatResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronStatResponses where
  parseJSON (Object o) = do
    leuronStatResponses <- o .: ("leuron_stat_responses" :: Text)
    pure $ LeuronStatResponses {
      leuronStatResponses = leuronStatResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronStatResponses where
  toJSON LeuronStatResponses{..} = object $
    [ "tag" .= ("LeuronStatResponses" :: Text)
    , "leuron_stat_responses" .= leuronStatResponses
    ]


instance Eq LeuronStatResponses where
  (==) a b = leuronStatResponses a == leuronStatResponses b

instance Show LeuronStatResponses where
    show rec = "leuronStatResponses: " <> show (leuronStatResponses rec)

data LeuronData
  = LnFact !(Fact)
  | LnFactList !(FactList)
  | LnCard !(Card)
  | LnDCard !(DCard)
  | LnDCardX !(DCardX)
  | LnAcronym !(Acronym)
  | LnSynonym !(Synonym)
  | LnAntonym !(Antonym)
  | LnTemplate !(Template)
  | LnImageAssociation !(ImageAssociation)
  | LnLinearDemo !(LinearDemo)
  | LnTable !(Table)
  | LnScript !(Script)
  | LnQA !(QA)
  | LnExamples 
  | LnEmpty 
  deriving (Generic,Typeable,NFData)


instance FromJSON LeuronData where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("LnFact" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> LnFact <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: LnFact"

      ("LnFactList" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> LnFactList <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: LnFactList"

      ("LnCard" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> LnCard <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: LnCard"

      ("LnDCard" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> LnDCard <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: LnDCard"

      ("LnDCardX" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> LnDCardX <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: LnDCardX"

      ("LnAcronym" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> LnAcronym <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: LnAcronym"

      ("LnSynonym" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> LnSynonym <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: LnSynonym"

      ("LnAntonym" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> LnAntonym <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: LnAntonym"

      ("LnTemplate" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> LnTemplate <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: LnTemplate"

      ("LnImageAssociation" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> LnImageAssociation <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: LnImageAssociation"

      ("LnLinearDemo" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> LnLinearDemo <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: LnLinearDemo"

      ("LnTable" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> LnTable <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: LnTable"

      ("LnScript" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> LnScript <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: LnScript"

      ("LnQA" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> LnQA <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: LnQA"

      ("LnExamples" :: Text) -> do
        pure LnExamples

      ("LnEmpty" :: Text) -> do
        pure LnEmpty

      _ -> fail "Could not parse LeuronData"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LeuronData where
  toJSON (LnFact x0) = object $
    [ "tag" .= ("LnFact" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (LnFactList x0) = object $
    [ "tag" .= ("LnFactList" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (LnCard x0) = object $
    [ "tag" .= ("LnCard" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (LnDCard x0) = object $
    [ "tag" .= ("LnDCard" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (LnDCardX x0) = object $
    [ "tag" .= ("LnDCardX" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (LnAcronym x0) = object $
    [ "tag" .= ("LnAcronym" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (LnSynonym x0) = object $
    [ "tag" .= ("LnSynonym" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (LnAntonym x0) = object $
    [ "tag" .= ("LnAntonym" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (LnTemplate x0) = object $
    [ "tag" .= ("LnTemplate" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (LnImageAssociation x0) = object $
    [ "tag" .= ("LnImageAssociation" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (LnLinearDemo x0) = object $
    [ "tag" .= ("LnLinearDemo" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (LnTable x0) = object $
    [ "tag" .= ("LnTable" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (LnScript x0) = object $
    [ "tag" .= ("LnScript" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (LnQA x0) = object $
    [ "tag" .= ("LnQA" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (LnExamples ) = object $
    [ "tag" .= ("LnExamples" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (LnEmpty ) = object $
    [ "tag" .= ("LnEmpty" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq LeuronData where
  (==) (LnFact x0a) (LnFact x0b) = x0a == x0b
  (==) (LnFactList x0a) (LnFactList x0b) = x0a == x0b
  (==) (LnCard x0a) (LnCard x0b) = x0a == x0b
  (==) (LnDCard x0a) (LnDCard x0b) = x0a == x0b
  (==) (LnDCardX x0a) (LnDCardX x0b) = x0a == x0b
  (==) (LnAcronym x0a) (LnAcronym x0b) = x0a == x0b
  (==) (LnSynonym x0a) (LnSynonym x0b) = x0a == x0b
  (==) (LnAntonym x0a) (LnAntonym x0b) = x0a == x0b
  (==) (LnTemplate x0a) (LnTemplate x0b) = x0a == x0b
  (==) (LnImageAssociation x0a) (LnImageAssociation x0b) = x0a == x0b
  (==) (LnLinearDemo x0a) (LnLinearDemo x0b) = x0a == x0b
  (==) (LnTable x0a) (LnTable x0b) = x0a == x0b
  (==) (LnScript x0a) (LnScript x0b) = x0a == x0b
  (==) (LnQA x0a) (LnQA x0b) = x0a == x0b
  (==) LnExamples LnExamples = True
  (==) LnEmpty LnEmpty = True
  (==) _ _ = False

instance Show LeuronData where
  show (LnFact x0) = "ln_fact: " <> show x0
  show (LnFactList x0) = "ln_fact_list: " <> show x0
  show (LnCard x0) = "ln_card: " <> show x0
  show (LnDCard x0) = "ln_dcard: " <> show x0
  show (LnDCardX x0) = "ln_dcard_x: " <> show x0
  show (LnAcronym x0) = "ln_acronym: " <> show x0
  show (LnSynonym x0) = "ln_synonym: " <> show x0
  show (LnAntonym x0) = "ln_antonym: " <> show x0
  show (LnTemplate x0) = "ln_template: " <> show x0
  show (LnImageAssociation x0) = "ln_image_association: " <> show x0
  show (LnLinearDemo x0) = "ln_linear_demo: " <> show x0
  show (LnTable x0) = "ln_table: " <> show x0
  show (LnScript x0) = "ln_script: " <> show x0
  show (LnQA x0) = "ln_qa: " <> show x0
  show LnExamples = "ln_examples"
  show LnEmpty = "ln_empty"


data TyLeuron
  = TyLnFact 
  | TyLnFactList 
  | TyLnCard 
  | TyLnDCard 
  | TyLnDCardX 
  | TyLnAcronym 
  | TyLnSynonym 
  | TyLnAntonym 
  | TyLnTemplate 
  | TyLnImageAssociation 
  | TyLnLinearDemo 
  | TyLnTable 
  | TyLnScript 
  | TyLnQA 
  | TyLnExamples 
  | TyLnEmpty 
  deriving (Generic,Typeable,NFData)


instance FromJSON TyLeuron where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("TyLnFact" :: Text) -> do
        pure TyLnFact

      ("TyLnFactList" :: Text) -> do
        pure TyLnFactList

      ("TyLnCard" :: Text) -> do
        pure TyLnCard

      ("TyLnDCard" :: Text) -> do
        pure TyLnDCard

      ("TyLnDCardX" :: Text) -> do
        pure TyLnDCardX

      ("TyLnAcronym" :: Text) -> do
        pure TyLnAcronym

      ("TyLnSynonym" :: Text) -> do
        pure TyLnSynonym

      ("TyLnAntonym" :: Text) -> do
        pure TyLnAntonym

      ("TyLnTemplate" :: Text) -> do
        pure TyLnTemplate

      ("TyLnImageAssociation" :: Text) -> do
        pure TyLnImageAssociation

      ("TyLnLinearDemo" :: Text) -> do
        pure TyLnLinearDemo

      ("TyLnTable" :: Text) -> do
        pure TyLnTable

      ("TyLnScript" :: Text) -> do
        pure TyLnScript

      ("TyLnQA" :: Text) -> do
        pure TyLnQA

      ("TyLnExamples" :: Text) -> do
        pure TyLnExamples

      ("TyLnEmpty" :: Text) -> do
        pure TyLnEmpty

      _ -> fail "Could not parse TyLeuron"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TyLeuron where
  toJSON (TyLnFact ) = object $
    [ "tag" .= ("TyLnFact" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnFactList ) = object $
    [ "tag" .= ("TyLnFactList" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnCard ) = object $
    [ "tag" .= ("TyLnCard" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnDCard ) = object $
    [ "tag" .= ("TyLnDCard" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnDCardX ) = object $
    [ "tag" .= ("TyLnDCardX" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnAcronym ) = object $
    [ "tag" .= ("TyLnAcronym" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnSynonym ) = object $
    [ "tag" .= ("TyLnSynonym" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnAntonym ) = object $
    [ "tag" .= ("TyLnAntonym" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnTemplate ) = object $
    [ "tag" .= ("TyLnTemplate" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnImageAssociation ) = object $
    [ "tag" .= ("TyLnImageAssociation" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnLinearDemo ) = object $
    [ "tag" .= ("TyLnLinearDemo" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnTable ) = object $
    [ "tag" .= ("TyLnTable" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnScript ) = object $
    [ "tag" .= ("TyLnScript" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnQA ) = object $
    [ "tag" .= ("TyLnQA" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnExamples ) = object $
    [ "tag" .= ("TyLnExamples" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyLnEmpty ) = object $
    [ "tag" .= ("TyLnEmpty" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq TyLeuron where
  (==) TyLnFact TyLnFact = True
  (==) TyLnFactList TyLnFactList = True
  (==) TyLnCard TyLnCard = True
  (==) TyLnDCard TyLnDCard = True
  (==) TyLnDCardX TyLnDCardX = True
  (==) TyLnAcronym TyLnAcronym = True
  (==) TyLnSynonym TyLnSynonym = True
  (==) TyLnAntonym TyLnAntonym = True
  (==) TyLnTemplate TyLnTemplate = True
  (==) TyLnImageAssociation TyLnImageAssociation = True
  (==) TyLnLinearDemo TyLnLinearDemo = True
  (==) TyLnTable TyLnTable = True
  (==) TyLnScript TyLnScript = True
  (==) TyLnQA TyLnQA = True
  (==) TyLnExamples TyLnExamples = True
  (==) TyLnEmpty TyLnEmpty = True
  (==) _ _ = False

instance Show TyLeuron where
  show TyLnFact = "ty_ln_fact"
  show TyLnFactList = "ty_ln_fact_list"
  show TyLnCard = "ty_ln_card"
  show TyLnDCard = "ty_ln_dcard"
  show TyLnDCardX = "ty_ln_dcard_x"
  show TyLnAcronym = "ty_ln_acronym"
  show TyLnSynonym = "ty_ln_synonym"
  show TyLnAntonym = "ty_ln_antonym"
  show TyLnTemplate = "ty_ln_template"
  show TyLnImageAssociation = "ty_ln_image_association"
  show TyLnLinearDemo = "ty_ln_linear_demo"
  show TyLnTable = "ty_ln_table"
  show TyLnScript = "ty_ln_script"
  show TyLnQA = "ty_ln_qa"
  show TyLnExamples = "ty_ln_examples"
  show TyLnEmpty = "ty_ln_empty"


data Fact = Fact {
  factText :: !(Text)
}  deriving (Generic,Typeable,NFData)


instance FromJSON Fact where
  parseJSON (Object o) = do
    factText <- o .: ("text" :: Text)
    pure $ Fact {
      factText = factText
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Fact where
  toJSON Fact{..} = object $
    [ "tag" .= ("Fact" :: Text)
    , "text" .= factText
    ]


instance Eq Fact where
  (==) a b = factText a == factText b

instance Show Fact where
    show rec = "factText: " <> show (factText rec)

data FactList = FactList {
  factListFact :: !(Text),
  factListList :: !([Text])
}  deriving (Generic,Typeable,NFData)


instance FromJSON FactList where
  parseJSON (Object o) = do
    factListFact <- o .: ("fact" :: Text)
    factListList <- o .: ("list" :: Text)
    pure $ FactList {
      factListFact = factListFact,
      factListList = factListList
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON FactList where
  toJSON FactList{..} = object $
    [ "tag" .= ("FactList" :: Text)
    , "fact" .= factListFact
    , "list" .= factListList
    ]


instance Eq FactList where
  (==) a b = factListFact a == factListFact b && factListList a == factListList b

instance Show FactList where
    show rec = "factListFact: " <> show (factListFact rec) <> ", " <> "factListList: " <> show (factListList rec)

data Card = Card {
  cardFront :: !(Text),
  cardBack :: !(Text)
}  deriving (Generic,Typeable,NFData)


instance FromJSON Card where
  parseJSON (Object o) = do
    cardFront <- o .: ("front" :: Text)
    cardBack <- o .: ("back" :: Text)
    pure $ Card {
      cardFront = cardFront,
      cardBack = cardBack
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Card where
  toJSON Card{..} = object $
    [ "tag" .= ("Card" :: Text)
    , "front" .= cardFront
    , "back" .= cardBack
    ]


instance Eq Card where
  (==) a b = cardFront a == cardFront b && cardBack a == cardBack b

instance Show Card where
    show rec = "cardFront: " <> show (cardFront rec) <> ", " <> "cardBack: " <> show (cardBack rec)

data DCard = DCard {
  dcardFront :: !(Text),
  dcardBack :: !(Text)
}  deriving (Generic,Typeable,NFData)


instance FromJSON DCard where
  parseJSON (Object o) = do
    dcardFront <- o .: ("front" :: Text)
    dcardBack <- o .: ("back" :: Text)
    pure $ DCard {
      dcardFront = dcardFront,
      dcardBack = dcardBack
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON DCard where
  toJSON DCard{..} = object $
    [ "tag" .= ("DCard" :: Text)
    , "front" .= dcardFront
    , "back" .= dcardBack
    ]


instance Eq DCard where
  (==) a b = dcardFront a == dcardFront b && dcardBack a == dcardBack b

instance Show DCard where
    show rec = "dcardFront: " <> show (dcardFront rec) <> ", " <> "dcardBack: " <> show (dcardBack rec)

data DCardX = DCardX {
  dcardxFront :: !([Text]),
  dcardxBack :: !([Text])
}  deriving (Generic,Typeable,NFData)


instance FromJSON DCardX where
  parseJSON (Object o) = do
    dcardxFront <- o .: ("front" :: Text)
    dcardxBack <- o .: ("back" :: Text)
    pure $ DCardX {
      dcardxFront = dcardxFront,
      dcardxBack = dcardxBack
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON DCardX where
  toJSON DCardX{..} = object $
    [ "tag" .= ("DCardX" :: Text)
    , "front" .= dcardxFront
    , "back" .= dcardxBack
    ]


instance Eq DCardX where
  (==) a b = dcardxFront a == dcardxFront b && dcardxBack a == dcardxBack b

instance Show DCardX where
    show rec = "dcardxFront: " <> show (dcardxFront rec) <> ", " <> "dcardxBack: " <> show (dcardxBack rec)

data Acronym = Acronym {
  acronymAbbreviation :: !(Text),
  acronymMeaning :: !(Text)
}  deriving (Generic,Typeable,NFData)


instance FromJSON Acronym where
  parseJSON (Object o) = do
    acronymAbbreviation <- o .: ("abbreviation" :: Text)
    acronymMeaning <- o .: ("meaning" :: Text)
    pure $ Acronym {
      acronymAbbreviation = acronymAbbreviation,
      acronymMeaning = acronymMeaning
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Acronym where
  toJSON Acronym{..} = object $
    [ "tag" .= ("Acronym" :: Text)
    , "abbreviation" .= acronymAbbreviation
    , "meaning" .= acronymMeaning
    ]


instance Eq Acronym where
  (==) a b = acronymAbbreviation a == acronymAbbreviation b && acronymMeaning a == acronymMeaning b

instance Show Acronym where
    show rec = "acronymAbbreviation: " <> show (acronymAbbreviation rec) <> ", " <> "acronymMeaning: " <> show (acronymMeaning rec)

data Synonym = Synonym {
  synonymA :: !(Text),
  synonymB :: !(Text)
}  deriving (Generic,Typeable,NFData)


instance FromJSON Synonym where
  parseJSON (Object o) = do
    synonymA <- o .: ("a" :: Text)
    synonymB <- o .: ("b" :: Text)
    pure $ Synonym {
      synonymA = synonymA,
      synonymB = synonymB
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Synonym where
  toJSON Synonym{..} = object $
    [ "tag" .= ("Synonym" :: Text)
    , "a" .= synonymA
    , "b" .= synonymB
    ]


instance Eq Synonym where
  (==) a b = synonymA a == synonymA b && synonymB a == synonymB b

instance Show Synonym where
    show rec = "synonymA: " <> show (synonymA rec) <> ", " <> "synonymB: " <> show (synonymB rec)

data Antonym = Antonym {
  antonymA :: !(Text),
  antonymB :: !(Text)
}  deriving (Generic,Typeable,NFData)


instance FromJSON Antonym where
  parseJSON (Object o) = do
    antonymA <- o .: ("a" :: Text)
    antonymB <- o .: ("b" :: Text)
    pure $ Antonym {
      antonymA = antonymA,
      antonymB = antonymB
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Antonym where
  toJSON Antonym{..} = object $
    [ "tag" .= ("Antonym" :: Text)
    , "a" .= antonymA
    , "b" .= antonymB
    ]


instance Eq Antonym where
  (==) a b = antonymA a == antonymA b && antonymB a == antonymB b

instance Show Antonym where
    show rec = "antonymA: " <> show (antonymA rec) <> ", " <> "antonymB: " <> show (antonymB rec)

data Template = Template {
  template :: !(Text),
  templateValues :: !([TemplateValue])
}  deriving (Generic,Typeable,NFData)


instance FromJSON Template where
  parseJSON (Object o) = do
    template <- o .: ("template" :: Text)
    templateValues <- o .: ("values" :: Text)
    pure $ Template {
      template = template,
      templateValues = templateValues
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Template where
  toJSON Template{..} = object $
    [ "tag" .= ("Template" :: Text)
    , "template" .= template
    , "values" .= templateValues
    ]


instance Eq Template where
  (==) a b = template a == template b && templateValues a == templateValues b

instance Show Template where
    show rec = "template: " <> show (template rec) <> ", " <> "templateValues: " <> show (templateValues rec)

type TemplateValue  = (((,) Text) [Text])


data ImageAssociation = ImageAssociation {
  imageUrl :: !([Text]),
  assocBy :: !([Text]),
  assocResult :: !([Text])
}  deriving (Generic,Typeable,NFData)


instance FromJSON ImageAssociation where
  parseJSON (Object o) = do
    imageUrl <- o .: ("image_url" :: Text)
    assocBy <- o .: ("assoc_by" :: Text)
    assocResult <- o .: ("assoc_result" :: Text)
    pure $ ImageAssociation {
      imageUrl = imageUrl,
      assocBy = assocBy,
      assocResult = assocResult
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ImageAssociation where
  toJSON ImageAssociation{..} = object $
    [ "tag" .= ("ImageAssociation" :: Text)
    , "image_url" .= imageUrl
    , "assoc_by" .= assocBy
    , "assoc_result" .= assocResult
    ]


instance Eq ImageAssociation where
  (==) a b = imageUrl a == imageUrl b && assocBy a == assocBy b && assocResult a == assocResult b

instance Show ImageAssociation where
    show rec = "imageUrl: " <> show (imageUrl rec) <> ", " <> "assocBy: " <> show (assocBy rec) <> ", " <> "assocResult: " <> show (assocResult rec)

data Script = Script {
  scriptTitle :: !(Text),
  scriptDesc :: !(Text),
  scriptUrl :: !(Text)
}  deriving (Generic,Typeable,NFData)


instance FromJSON Script where
  parseJSON (Object o) = do
    scriptTitle <- o .: ("title" :: Text)
    scriptDesc <- o .: ("desc" :: Text)
    scriptUrl <- o .: ("url" :: Text)
    pure $ Script {
      scriptTitle = scriptTitle,
      scriptDesc = scriptDesc,
      scriptUrl = scriptUrl
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Script where
  toJSON Script{..} = object $
    [ "tag" .= ("Script" :: Text)
    , "title" .= scriptTitle
    , "desc" .= scriptDesc
    , "url" .= scriptUrl
    ]


instance Eq Script where
  (==) a b = scriptTitle a == scriptTitle b && scriptDesc a == scriptDesc b && scriptUrl a == scriptUrl b

instance Show Script where
    show rec = "scriptTitle: " <> show (scriptTitle rec) <> ", " <> "scriptDesc: " <> show (scriptDesc rec) <> ", " <> "scriptUrl: " <> show (scriptUrl rec)

type LDContent  = Text


type LDHint  = (Maybe Text)


type LinearDemoNode  = (((,) LDContent) LDHint)


data LinearDemo = LinearDemo {
  linearDemoLabel :: !(Text),
  linearDemoContent :: !([LinearDemoNode])
}  deriving (Generic,Typeable,NFData)


instance FromJSON LinearDemo where
  parseJSON (Object o) = do
    linearDemoLabel <- o .: ("label" :: Text)
    linearDemoContent <- o .: ("content" :: Text)
    pure $ LinearDemo {
      linearDemoLabel = linearDemoLabel,
      linearDemoContent = linearDemoContent
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON LinearDemo where
  toJSON LinearDemo{..} = object $
    [ "tag" .= ("LinearDemo" :: Text)
    , "label" .= linearDemoLabel
    , "content" .= linearDemoContent
    ]


instance Eq LinearDemo where
  (==) a b = linearDemoLabel a == linearDemoLabel b && linearDemoContent a == linearDemoContent b

instance Show LinearDemo where
    show rec = "linearDemoLabel: " <> show (linearDemoLabel rec) <> ", " <> "linearDemoContent: " <> show (linearDemoContent rec)

data QA = QA {
  qaQuestion :: !(Text),
  qaAnswer :: !(Text)
}  deriving (Generic,Typeable,NFData)


instance FromJSON QA where
  parseJSON (Object o) = do
    qaQuestion <- o .: ("question" :: Text)
    qaAnswer <- o .: ("answer" :: Text)
    pure $ QA {
      qaQuestion = qaQuestion,
      qaAnswer = qaAnswer
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON QA where
  toJSON QA{..} = object $
    [ "tag" .= ("QA" :: Text)
    , "question" .= qaQuestion
    , "answer" .= qaAnswer
    ]


instance Eq QA where
  (==) a b = qaQuestion a == qaQuestion b && qaAnswer a == qaAnswer b

instance Show QA where
    show rec = "qaQuestion: " <> show (qaQuestion rec) <> ", " <> "qaAnswer: " <> show (qaAnswer rec)

data Table = Table {
  tableTitle :: !(Text),
  tableColumns :: !([Text]),
  tableRows :: !([[(Maybe Text)]])
}  deriving (Generic,Typeable,NFData)


instance FromJSON Table where
  parseJSON (Object o) = do
    tableTitle <- o .: ("title" :: Text)
    tableColumns <- o .: ("columns" :: Text)
    tableRows <- o .: ("rows" :: Text)
    pure $ Table {
      tableTitle = tableTitle,
      tableColumns = tableColumns,
      tableRows = tableRows
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Table where
  toJSON Table{..} = object $
    [ "tag" .= ("Table" :: Text)
    , "title" .= tableTitle
    , "columns" .= tableColumns
    , "rows" .= tableRows
    ]


instance Eq Table where
  (==) a b = tableTitle a == tableTitle b && tableColumns a == tableColumns b && tableRows a == tableRows b

instance Show Table where
    show rec = "tableTitle: " <> show (tableTitle rec) <> ", " <> "tableColumns: " <> show (tableColumns rec) <> ", " <> "tableRows: " <> show (tableRows rec)
-- footer