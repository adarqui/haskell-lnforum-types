{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Training where





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

data TrainingNode = TrainingNode {
  numTotal :: !(Int64),
  numKnow :: !(Int64),
  numDontKnow :: !(Int64),
  numDontCare :: !(Int64),
  numProtest :: !(Int64),
  honorKnow :: !(Int64),
  honorDontKnow :: !(Int64),
  honorDontCare :: !(Int64),
  honorProtest :: !(Int64),
  honorKnowAt :: !((Maybe UTCTime)),
  honorDontKnowAt :: !((Maybe UTCTime)),
  honorDontCareAt :: !((Maybe UTCTime)),
  honorProtestAt :: !((Maybe UTCTime)),
  booleanKnow :: !(Int64),
  booleanDontKnow :: !(Int64),
  booleanDontCare :: !(Int64),
  booleanProtest :: !(Int64),
  booleanKnowAt :: !((Maybe UTCTime)),
  booleanDontKnowAt :: !((Maybe UTCTime)),
  booleanDontCareAt :: !((Maybe UTCTime)),
  booleanProtestAt :: !((Maybe UTCTime)),
  matchKnow :: !(Int64),
  matchDontKnow :: !(Int64),
  matchDontCare :: !(Int64),
  matchProtest :: !(Int64),
  matchKnowAt :: !((Maybe UTCTime)),
  matchDontKnowAt :: !((Maybe UTCTime)),
  matchDontCareAt :: !((Maybe UTCTime)),
  matchProtestAt :: !((Maybe UTCTime)),
  subsKnow :: !(Int64),
  subsDontKnow :: !(Int64),
  subsDontCare :: !(Int64),
  subsProtest :: !(Int64),
  subsKnowAt :: !((Maybe UTCTime)),
  subsDontKnowAt :: !((Maybe UTCTime)),
  subsDontCareAt :: !((Maybe UTCTime)),
  subsProtestAt :: !((Maybe UTCTime)),
  splitsKnow :: !(Int64),
  splitsDontKnow :: !(Int64),
  splitsDontCare :: !(Int64),
  splitsProtest :: !(Int64),
  splitsKnowAt :: !((Maybe UTCTime)),
  splitsDontKnowAt :: !((Maybe UTCTime)),
  splitsDontCareAt :: !((Maybe UTCTime)),
  splitsProtestAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON TrainingNode where
  parseJSON (Object o) = do
    numTotal <- o .: ("num_total" :: Text)
    numKnow <- o .: ("num_know" :: Text)
    numDontKnow <- o .: ("num_dont_know" :: Text)
    numDontCare <- o .: ("num_dont_care" :: Text)
    numProtest <- o .: ("num_protest" :: Text)
    honorKnow <- o .: ("honor_know" :: Text)
    honorDontKnow <- o .: ("honor_dont_know" :: Text)
    honorDontCare <- o .: ("honor_dont_care" :: Text)
    honorProtest <- o .: ("honor_protest" :: Text)
    honorKnowAt <- o .: ("honor_know_at" :: Text)
    honorDontKnowAt <- o .: ("honor_dont_know_at" :: Text)
    honorDontCareAt <- o .: ("honor_dont_care_at" :: Text)
    honorProtestAt <- o .: ("honor_protest_at" :: Text)
    booleanKnow <- o .: ("boolean_know" :: Text)
    booleanDontKnow <- o .: ("boolean_dont_know" :: Text)
    booleanDontCare <- o .: ("boolean_dont_care" :: Text)
    booleanProtest <- o .: ("boolean_protest" :: Text)
    booleanKnowAt <- o .: ("boolean_know_at" :: Text)
    booleanDontKnowAt <- o .: ("boolean_dont_know_at" :: Text)
    booleanDontCareAt <- o .: ("boolean_dont_care_at" :: Text)
    booleanProtestAt <- o .: ("boolean_protest_at" :: Text)
    matchKnow <- o .: ("match_know" :: Text)
    matchDontKnow <- o .: ("match_dont_know" :: Text)
    matchDontCare <- o .: ("match_dont_care" :: Text)
    matchProtest <- o .: ("match_protest" :: Text)
    matchKnowAt <- o .: ("match_know_at" :: Text)
    matchDontKnowAt <- o .: ("match_dont_know_at" :: Text)
    matchDontCareAt <- o .: ("match_dont_care_at" :: Text)
    matchProtestAt <- o .: ("match_protest_at" :: Text)
    subsKnow <- o .: ("subs_know" :: Text)
    subsDontKnow <- o .: ("subs_dont_know" :: Text)
    subsDontCare <- o .: ("subs_dont_care" :: Text)
    subsProtest <- o .: ("subs_protest" :: Text)
    subsKnowAt <- o .: ("subs_know_at" :: Text)
    subsDontKnowAt <- o .: ("subs_dont_know_at" :: Text)
    subsDontCareAt <- o .: ("subs_dont_care_at" :: Text)
    subsProtestAt <- o .: ("subs_protest_at" :: Text)
    splitsKnow <- o .: ("splits_know" :: Text)
    splitsDontKnow <- o .: ("splits_dont_know" :: Text)
    splitsDontCare <- o .: ("splits_dont_care" :: Text)
    splitsProtest <- o .: ("splits_protest" :: Text)
    splitsKnowAt <- o .: ("splits_know_at" :: Text)
    splitsDontKnowAt <- o .: ("splits_dont_know_at" :: Text)
    splitsDontCareAt <- o .: ("splits_dont_care_at" :: Text)
    splitsProtestAt <- o .: ("splits_protest_at" :: Text)
    pure $ TrainingNode {
      numTotal = numTotal,
      numKnow = numKnow,
      numDontKnow = numDontKnow,
      numDontCare = numDontCare,
      numProtest = numProtest,
      honorKnow = honorKnow,
      honorDontKnow = honorDontKnow,
      honorDontCare = honorDontCare,
      honorProtest = honorProtest,
      honorKnowAt = honorKnowAt,
      honorDontKnowAt = honorDontKnowAt,
      honorDontCareAt = honorDontCareAt,
      honorProtestAt = honorProtestAt,
      booleanKnow = booleanKnow,
      booleanDontKnow = booleanDontKnow,
      booleanDontCare = booleanDontCare,
      booleanProtest = booleanProtest,
      booleanKnowAt = booleanKnowAt,
      booleanDontKnowAt = booleanDontKnowAt,
      booleanDontCareAt = booleanDontCareAt,
      booleanProtestAt = booleanProtestAt,
      matchKnow = matchKnow,
      matchDontKnow = matchDontKnow,
      matchDontCare = matchDontCare,
      matchProtest = matchProtest,
      matchKnowAt = matchKnowAt,
      matchDontKnowAt = matchDontKnowAt,
      matchDontCareAt = matchDontCareAt,
      matchProtestAt = matchProtestAt,
      subsKnow = subsKnow,
      subsDontKnow = subsDontKnow,
      subsDontCare = subsDontCare,
      subsProtest = subsProtest,
      subsKnowAt = subsKnowAt,
      subsDontKnowAt = subsDontKnowAt,
      subsDontCareAt = subsDontCareAt,
      subsProtestAt = subsProtestAt,
      splitsKnow = splitsKnow,
      splitsDontKnow = splitsDontKnow,
      splitsDontCare = splitsDontCare,
      splitsProtest = splitsProtest,
      splitsKnowAt = splitsKnowAt,
      splitsDontKnowAt = splitsDontKnowAt,
      splitsDontCareAt = splitsDontCareAt,
      splitsProtestAt = splitsProtestAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TrainingNode where
  toJSON TrainingNode{..} = object $
    [ "tag" .= ("TrainingNode" :: Text)
    , "num_total" .= numTotal
    , "num_know" .= numKnow
    , "num_dont_know" .= numDontKnow
    , "num_dont_care" .= numDontCare
    , "num_protest" .= numProtest
    , "honor_know" .= honorKnow
    , "honor_dont_know" .= honorDontKnow
    , "honor_dont_care" .= honorDontCare
    , "honor_protest" .= honorProtest
    , "honor_know_at" .= honorKnowAt
    , "honor_dont_know_at" .= honorDontKnowAt
    , "honor_dont_care_at" .= honorDontCareAt
    , "honor_protest_at" .= honorProtestAt
    , "boolean_know" .= booleanKnow
    , "boolean_dont_know" .= booleanDontKnow
    , "boolean_dont_care" .= booleanDontCare
    , "boolean_protest" .= booleanProtest
    , "boolean_know_at" .= booleanKnowAt
    , "boolean_dont_know_at" .= booleanDontKnowAt
    , "boolean_dont_care_at" .= booleanDontCareAt
    , "boolean_protest_at" .= booleanProtestAt
    , "match_know" .= matchKnow
    , "match_dont_know" .= matchDontKnow
    , "match_dont_care" .= matchDontCare
    , "match_protest" .= matchProtest
    , "match_know_at" .= matchKnowAt
    , "match_dont_know_at" .= matchDontKnowAt
    , "match_dont_care_at" .= matchDontCareAt
    , "match_protest_at" .= matchProtestAt
    , "subs_know" .= subsKnow
    , "subs_dont_know" .= subsDontKnow
    , "subs_dont_care" .= subsDontCare
    , "subs_protest" .= subsProtest
    , "subs_know_at" .= subsKnowAt
    , "subs_dont_know_at" .= subsDontKnowAt
    , "subs_dont_care_at" .= subsDontCareAt
    , "subs_protest_at" .= subsProtestAt
    , "splits_know" .= splitsKnow
    , "splits_dont_know" .= splitsDontKnow
    , "splits_dont_care" .= splitsDontCare
    , "splits_protest" .= splitsProtest
    , "splits_know_at" .= splitsKnowAt
    , "splits_dont_know_at" .= splitsDontKnowAt
    , "splits_dont_care_at" .= splitsDontCareAt
    , "splits_protest_at" .= splitsProtestAt
    ]


instance Eq TrainingNode where
  (==) a b = numTotal a == numTotal b && numKnow a == numKnow b && numDontKnow a == numDontKnow b && numDontCare a == numDontCare b && numProtest a == numProtest b && honorKnow a == honorKnow b && honorDontKnow a == honorDontKnow b && honorDontCare a == honorDontCare b && honorProtest a == honorProtest b && honorKnowAt a == honorKnowAt b && honorDontKnowAt a == honorDontKnowAt b && honorDontCareAt a == honorDontCareAt b && honorProtestAt a == honorProtestAt b && booleanKnow a == booleanKnow b && booleanDontKnow a == booleanDontKnow b && booleanDontCare a == booleanDontCare b && booleanProtest a == booleanProtest b && booleanKnowAt a == booleanKnowAt b && booleanDontKnowAt a == booleanDontKnowAt b && booleanDontCareAt a == booleanDontCareAt b && booleanProtestAt a == booleanProtestAt b && matchKnow a == matchKnow b && matchDontKnow a == matchDontKnow b && matchDontCare a == matchDontCare b && matchProtest a == matchProtest b && matchKnowAt a == matchKnowAt b && matchDontKnowAt a == matchDontKnowAt b && matchDontCareAt a == matchDontCareAt b && matchProtestAt a == matchProtestAt b && subsKnow a == subsKnow b && subsDontKnow a == subsDontKnow b && subsDontCare a == subsDontCare b && subsProtest a == subsProtest b && subsKnowAt a == subsKnowAt b && subsDontKnowAt a == subsDontKnowAt b && subsDontCareAt a == subsDontCareAt b && subsProtestAt a == subsProtestAt b && splitsKnow a == splitsKnow b && splitsDontKnow a == splitsDontKnow b && splitsDontCare a == splitsDontCare b && splitsProtest a == splitsProtest b && splitsKnowAt a == splitsKnowAt b && splitsDontKnowAt a == splitsDontKnowAt b && splitsDontCareAt a == splitsDontCareAt b && splitsProtestAt a == splitsProtestAt b

instance Show TrainingNode where
    show rec = "numTotal: " <> show (numTotal rec) <> ", " <> "numKnow: " <> show (numKnow rec) <> ", " <> "numDontKnow: " <> show (numDontKnow rec) <> ", " <> "numDontCare: " <> show (numDontCare rec) <> ", " <> "numProtest: " <> show (numProtest rec) <> ", " <> "honorKnow: " <> show (honorKnow rec) <> ", " <> "honorDontKnow: " <> show (honorDontKnow rec) <> ", " <> "honorDontCare: " <> show (honorDontCare rec) <> ", " <> "honorProtest: " <> show (honorProtest rec) <> ", " <> "honorKnowAt: " <> show (honorKnowAt rec) <> ", " <> "honorDontKnowAt: " <> show (honorDontKnowAt rec) <> ", " <> "honorDontCareAt: " <> show (honorDontCareAt rec) <> ", " <> "honorProtestAt: " <> show (honorProtestAt rec) <> ", " <> "booleanKnow: " <> show (booleanKnow rec) <> ", " <> "booleanDontKnow: " <> show (booleanDontKnow rec) <> ", " <> "booleanDontCare: " <> show (booleanDontCare rec) <> ", " <> "booleanProtest: " <> show (booleanProtest rec) <> ", " <> "booleanKnowAt: " <> show (booleanKnowAt rec) <> ", " <> "booleanDontKnowAt: " <> show (booleanDontKnowAt rec) <> ", " <> "booleanDontCareAt: " <> show (booleanDontCareAt rec) <> ", " <> "booleanProtestAt: " <> show (booleanProtestAt rec) <> ", " <> "matchKnow: " <> show (matchKnow rec) <> ", " <> "matchDontKnow: " <> show (matchDontKnow rec) <> ", " <> "matchDontCare: " <> show (matchDontCare rec) <> ", " <> "matchProtest: " <> show (matchProtest rec) <> ", " <> "matchKnowAt: " <> show (matchKnowAt rec) <> ", " <> "matchDontKnowAt: " <> show (matchDontKnowAt rec) <> ", " <> "matchDontCareAt: " <> show (matchDontCareAt rec) <> ", " <> "matchProtestAt: " <> show (matchProtestAt rec) <> ", " <> "subsKnow: " <> show (subsKnow rec) <> ", " <> "subsDontKnow: " <> show (subsDontKnow rec) <> ", " <> "subsDontCare: " <> show (subsDontCare rec) <> ", " <> "subsProtest: " <> show (subsProtest rec) <> ", " <> "subsKnowAt: " <> show (subsKnowAt rec) <> ", " <> "subsDontKnowAt: " <> show (subsDontKnowAt rec) <> ", " <> "subsDontCareAt: " <> show (subsDontCareAt rec) <> ", " <> "subsProtestAt: " <> show (subsProtestAt rec) <> ", " <> "splitsKnow: " <> show (splitsKnow rec) <> ", " <> "splitsDontKnow: " <> show (splitsDontKnow rec) <> ", " <> "splitsDontCare: " <> show (splitsDontCare rec) <> ", " <> "splitsProtest: " <> show (splitsProtest rec) <> ", " <> "splitsKnowAt: " <> show (splitsKnowAt rec) <> ", " <> "splitsDontKnowAt: " <> show (splitsDontKnowAt rec) <> ", " <> "splitsDontCareAt: " <> show (splitsDontCareAt rec) <> ", " <> "splitsProtestAt: " <> show (splitsProtestAt rec)

data TrainingStyle
  = TS_Simple 
  | TS_Boolean 
  | TS_Matching 
  | TS_Subs 
  | TS_Splits 
  deriving (Generic,Typeable,NFData)


instance FromJSON TrainingStyle where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("TS_Simple" :: Text) -> do
        pure TS_Simple

      ("TS_Boolean" :: Text) -> do
        pure TS_Boolean

      ("TS_Matching" :: Text) -> do
        pure TS_Matching

      ("TS_Subs" :: Text) -> do
        pure TS_Subs

      ("TS_Splits" :: Text) -> do
        pure TS_Splits

      _ -> fail "Could not parse TrainingStyle"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TrainingStyle where
  toJSON (TS_Simple ) = object $
    [ "tag" .= ("TS_Simple" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TS_Boolean ) = object $
    [ "tag" .= ("TS_Boolean" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TS_Matching ) = object $
    [ "tag" .= ("TS_Matching" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TS_Subs ) = object $
    [ "tag" .= ("TS_Subs" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TS_Splits ) = object $
    [ "tag" .= ("TS_Splits" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq TrainingStyle where
  (==) TS_Simple TS_Simple = True
  (==) TS_Boolean TS_Boolean = True
  (==) TS_Matching TS_Matching = True
  (==) TS_Subs TS_Subs = True
  (==) TS_Splits TS_Splits = True
  (==) _ _ = False

instance Show TrainingStyle where
  show TS_Simple = "ts_simple"
  show TS_Boolean = "ts_boolean"
  show TS_Matching = "ts_matching"
  show TS_Subs = "ts_subs"
  show TS_Splits = "ts_splits"


instance Read TrainingStyle where
  readsPrec _ "ts_simple" = [(TS_Simple, "")]
  readsPrec _ "ts_boolean" = [(TS_Boolean, "")]
  readsPrec _ "ts_matching" = [(TS_Matching, "")]
  readsPrec _ "ts_subs" = [(TS_Subs, "")]
  readsPrec _ "ts_splits" = [(TS_Splits, "")]
  readsPrec _ _ = []

-- footer