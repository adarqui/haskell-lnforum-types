{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pm where




import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)

newtype PmRequest = PmRequest {
  pmRequestSubject :: Text,
  pmRequestBody :: Text,
  pmRequestGuard :: Int
}


instance FromJSON PmRequest where
  parseJSON (Object o) = do
    pmRequestSubject <- o .: ("subject" :: Text)
    pmRequestBody <- o .: ("body" :: Text)
    pmRequestGuard <- o .: ("guard" :: Text)
    pure $ PmRequest {
      pmRequestSubject = pmRequestSubject,
      pmRequestBody = pmRequestBody,
      pmRequestGuard = pmRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON PmRequest where
  toJSON PmRequest{..} = object $
    [ "tag" .= ("PmRequest" :: Text)
    , "subject" .= pmRequestSubject
    , "body" .= pmRequestBody
    , "guard" .= pmRequestGuard
    ]


instance Eq PmRequest where
  (==) a b = pmRequestSubject a == pmRequestSubject b && pmRequestBody a == pmRequestBody b && pmRequestGuard a == pmRequestGuard b

instance Show PmRequest where
    show rec = "pmRequestSubject: " <> show (pmRequestSubject rec) <> ", " <> "pmRequestBody: " <> show (pmRequestBody rec) <> ", " <> "pmRequestGuard: " <> show (pmRequestGuard rec)

newtype PmResponse = PmResponse {
  pmResponseId :: Int64,
  pmResponseUserId :: Int64,
  pmResponseToUserId :: Int64,
  pmResponseSubject :: Text,
  pmResponseBody :: Text,
  pmResponseActive :: Bool,
  pmResponseGuard :: Int,
  pmResponseCreatedAt :: (Maybe UTCTime),
  pmResponseModifiedAt :: (Maybe UTCTime),
  pmResponseActivityAt :: (Maybe UTCTime)
}


instance FromJSON PmResponse where
  parseJSON (Object o) = do
    pmResponseId <- o .: ("id" :: Text)
    pmResponseUserId <- o .: ("user_id" :: Text)
    pmResponseToUserId <- o .: ("to_user_id" :: Text)
    pmResponseSubject <- o .: ("subject" :: Text)
    pmResponseBody <- o .: ("body" :: Text)
    pmResponseActive <- o .: ("active" :: Text)
    pmResponseGuard <- o .: ("guard" :: Text)
    pmResponseCreatedAt <- o .: ("created_at" :: Text)
    pmResponseModifiedAt <- o .: ("modified_at" :: Text)
    pmResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ PmResponse {
      pmResponseId = pmResponseId,
      pmResponseUserId = pmResponseUserId,
      pmResponseToUserId = pmResponseToUserId,
      pmResponseSubject = pmResponseSubject,
      pmResponseBody = pmResponseBody,
      pmResponseActive = pmResponseActive,
      pmResponseGuard = pmResponseGuard,
      pmResponseCreatedAt = pmResponseCreatedAt,
      pmResponseModifiedAt = pmResponseModifiedAt,
      pmResponseActivityAt = pmResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON PmResponse where
  toJSON PmResponse{..} = object $
    [ "tag" .= ("PmResponse" :: Text)
    , "id" .= pmResponseId
    , "user_id" .= pmResponseUserId
    , "to_user_id" .= pmResponseToUserId
    , "subject" .= pmResponseSubject
    , "body" .= pmResponseBody
    , "active" .= pmResponseActive
    , "guard" .= pmResponseGuard
    , "created_at" .= pmResponseCreatedAt
    , "modified_at" .= pmResponseModifiedAt
    , "activity_at" .= pmResponseActivityAt
    ]


instance Eq PmResponse where
  (==) a b = pmResponseId a == pmResponseId b && pmResponseUserId a == pmResponseUserId b && pmResponseToUserId a == pmResponseToUserId b && pmResponseSubject a == pmResponseSubject b && pmResponseBody a == pmResponseBody b && pmResponseActive a == pmResponseActive b && pmResponseGuard a == pmResponseGuard b && pmResponseCreatedAt a == pmResponseCreatedAt b && pmResponseModifiedAt a == pmResponseModifiedAt b && pmResponseActivityAt a == pmResponseActivityAt b

instance Show PmResponse where
    show rec = "pmResponseId: " <> show (pmResponseId rec) <> ", " <> "pmResponseUserId: " <> show (pmResponseUserId rec) <> ", " <> "pmResponseToUserId: " <> show (pmResponseToUserId rec) <> ", " <> "pmResponseSubject: " <> show (pmResponseSubject rec) <> ", " <> "pmResponseBody: " <> show (pmResponseBody rec) <> ", " <> "pmResponseActive: " <> show (pmResponseActive rec) <> ", " <> "pmResponseGuard: " <> show (pmResponseGuard rec) <> ", " <> "pmResponseCreatedAt: " <> show (pmResponseCreatedAt rec) <> ", " <> "pmResponseModifiedAt: " <> show (pmResponseModifiedAt rec) <> ", " <> "pmResponseActivityAt: " <> show (pmResponseActivityAt rec)

newtype PmResponses = PmResponses {
  pmResponses :: [PmResponse]
}


instance FromJSON PmResponses where
  parseJSON (Object o) = do
    pmResponses <- o .: ("pm_responses" :: Text)
    pure $ PmResponses {
      pmResponses = pmResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON PmResponses where
  toJSON PmResponses{..} = object $
    [ "tag" .= ("PmResponses" :: Text)
    , "pm_responses" .= pmResponses
    ]


instance Eq PmResponses where
  (==) a b = pmResponses a == pmResponses b

instance Show PmResponses where
    show rec = "pmResponses: " <> show (pmResponses rec)
-- footer