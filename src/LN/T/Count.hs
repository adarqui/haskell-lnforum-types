{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Count where





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

data CountResponse = CountResponse {
  countResponseId :: !(Int64),
  countResponseN :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON CountResponse where
  parseJSON (Object o) = do
    countResponseId <- o .: ("id" :: Text)
    countResponseN <- o .: ("n" :: Text)
    pure $ CountResponse {
      countResponseId = countResponseId,
      countResponseN = countResponseN
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON CountResponse where
  toJSON CountResponse{..} = object $
    [ "tag" .= ("CountResponse" :: Text)
    , "id" .= countResponseId
    , "n" .= countResponseN
    ]


instance Eq CountResponse where
  (==) a b = countResponseId a == countResponseId b && countResponseN a == countResponseN b

instance Show CountResponse where
    show rec = "countResponseId: " <> show (countResponseId rec) <> ", " <> "countResponseN: " <> show (countResponseN rec)

data CountResponses = CountResponses {
  countResponses :: !([CountResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON CountResponses where
  parseJSON (Object o) = do
    countResponses <- o .: ("count_responses" :: Text)
    pure $ CountResponses {
      countResponses = countResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON CountResponses where
  toJSON CountResponses{..} = object $
    [ "tag" .= ("CountResponses" :: Text)
    , "count_responses" .= countResponses
    ]


instance Eq CountResponses where
  (==) a b = countResponses a == countResponses b

instance Show CountResponses where
    show rec = "countResponses: " <> show (countResponses rec)
-- footer