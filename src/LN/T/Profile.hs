{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Profile where


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

data ProfileX = ProfileX {
  profileLogin :: !(Text),
  profileName :: !(Text),
  profileEmail :: !(Text)
}  deriving (Generic,Typeable,NFData)


instance FromJSON ProfileX where
  parseJSON (Object o) = do
    profileLogin <- o .: ("profile_login" :: Text)
    profileName <- o .: ("profile_name" :: Text)
    profileEmail <- o .: ("profile_email" :: Text)
    pure $ ProfileX {
      profileLogin = profileLogin,
      profileName = profileName,
      profileEmail = profileEmail
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ProfileX where
  toJSON ProfileX{..} = object $
    [ "tag" .= ("ProfileX" :: Text)
    , "profile_login" .= profileLogin
    , "profile_name" .= profileName
    , "profile_email" .= profileEmail
    ]


instance Eq ProfileX where
  (==) a b = profileLogin a == profileLogin b && profileName a == profileName b && profileEmail a == profileEmail b

instance Show ProfileX where
    show rec = "profileLogin: " <> show (profileLogin rec) <> ", " <> "profileName: " <> show (profileName rec) <> ", " <> "profileEmail: " <> show (profileEmail rec)

data ProfileGender
  = GenderMale 
  | GenderFemale 
  | GenderUnknown 
  deriving (Generic,Typeable,NFData)


instance FromJSON ProfileGender where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("GenderMale" :: Text) -> do
        pure GenderMale

      ("GenderFemale" :: Text) -> do
        pure GenderFemale

      ("GenderUnknown" :: Text) -> do
        pure GenderUnknown

      _ -> fail "Could not parse ProfileGender"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ProfileGender where
  toJSON (GenderMale ) = object $
    [ "tag" .= ("GenderMale" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (GenderFemale ) = object $
    [ "tag" .= ("GenderFemale" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (GenderUnknown ) = object $
    [ "tag" .= ("GenderUnknown" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq ProfileGender where
  (==) GenderMale GenderMale = True
  (==) GenderFemale GenderFemale = True
  (==) GenderUnknown GenderUnknown = True
  (==) _ _ = False

instance Show ProfileGender where
  show GenderMale = "gender_male"
  show GenderFemale = "gender_female"
  show GenderUnknown = "gender_unknown"


instance Read ProfileGender where
  readsPrec _ "gender_male" = [(GenderMale, "")]
  readsPrec _ "gender_female" = [(GenderFemale, "")]
  readsPrec _ "gender_unknown" = [(GenderUnknown, "")]
  readsPrec _ _ = []


data ProfileRequest = ProfileRequest {
  profileRequestGender :: !(ProfileGender),
  profileRequestBirthdate :: !(UTCTime),
  profileRequestWebsite :: !((Maybe Text)),
  profileRequestWebsites :: !([Text]),
  profileRequestLocation :: !((Maybe Text)),
  profileRequestSignature :: !((Maybe Text)),
  profileRequestDebug :: !(Bool),
  profileRequestGuard :: !(Int),
  profileRequestStateWebsites :: !((Maybe Text))
}  deriving (Generic,Typeable,NFData)


instance FromJSON ProfileRequest where
  parseJSON (Object o) = do
    profileRequestGender <- o .: ("gender" :: Text)
    profileRequestBirthdate <- o .: ("birthdate" :: Text)
    profileRequestWebsite <- o .: ("website" :: Text)
    profileRequestWebsites <- o .: ("websites" :: Text)
    profileRequestLocation <- o .: ("location" :: Text)
    profileRequestSignature <- o .: ("signature" :: Text)
    profileRequestDebug <- o .: ("debug" :: Text)
    profileRequestGuard <- o .: ("guard" :: Text)
    profileRequestStateWebsites <- o .: ("state_websites" :: Text)
    pure $ ProfileRequest {
      profileRequestGender = profileRequestGender,
      profileRequestBirthdate = profileRequestBirthdate,
      profileRequestWebsite = profileRequestWebsite,
      profileRequestWebsites = profileRequestWebsites,
      profileRequestLocation = profileRequestLocation,
      profileRequestSignature = profileRequestSignature,
      profileRequestDebug = profileRequestDebug,
      profileRequestGuard = profileRequestGuard,
      profileRequestStateWebsites = profileRequestStateWebsites
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ProfileRequest where
  toJSON ProfileRequest{..} = object $
    [ "tag" .= ("ProfileRequest" :: Text)
    , "gender" .= profileRequestGender
    , "birthdate" .= profileRequestBirthdate
    , "website" .= profileRequestWebsite
    , "websites" .= profileRequestWebsites
    , "location" .= profileRequestLocation
    , "signature" .= profileRequestSignature
    , "debug" .= profileRequestDebug
    , "guard" .= profileRequestGuard
    , "state_websites" .= profileRequestStateWebsites
    ]


instance Eq ProfileRequest where
  (==) a b = profileRequestGender a == profileRequestGender b && profileRequestBirthdate a == profileRequestBirthdate b && profileRequestWebsite a == profileRequestWebsite b && profileRequestWebsites a == profileRequestWebsites b && profileRequestLocation a == profileRequestLocation b && profileRequestSignature a == profileRequestSignature b && profileRequestDebug a == profileRequestDebug b && profileRequestGuard a == profileRequestGuard b && profileRequestStateWebsites a == profileRequestStateWebsites b

instance Show ProfileRequest where
    show rec = "profileRequestGender: " <> show (profileRequestGender rec) <> ", " <> "profileRequestBirthdate: " <> show (profileRequestBirthdate rec) <> ", " <> "profileRequestWebsite: " <> show (profileRequestWebsite rec) <> ", " <> "profileRequestWebsites: " <> show (profileRequestWebsites rec) <> ", " <> "profileRequestLocation: " <> show (profileRequestLocation rec) <> ", " <> "profileRequestSignature: " <> show (profileRequestSignature rec) <> ", " <> "profileRequestDebug: " <> show (profileRequestDebug rec) <> ", " <> "profileRequestGuard: " <> show (profileRequestGuard rec) <> ", " <> "profileRequestStateWebsites: " <> show (profileRequestStateWebsites rec)

data ProfileResponse = ProfileResponse {
  profileResponseId :: !(Int64),
  profileResponseEnt :: !(Ent),
  profileResponseEntId :: !(Int64),
  profileResponseGender :: !(ProfileGender),
  profileResponseBirthdate :: !(UTCTime),
  profileResponseWebsite :: !((Maybe Text)),
  profileResponseLocation :: !((Maybe Text)),
  profileResponseSignature :: !((Maybe Text)),
  profileResponseDebug :: !(Bool),
  profileResponseKarmaGood :: !(Int),
  profileResponseKarmaBad :: !(Int),
  profileResponseGuard :: !(Int),
  profileResponseCreatedAt :: !((Maybe UTCTime)),
  profileResponseModifiedAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON ProfileResponse where
  parseJSON (Object o) = do
    profileResponseId <- o .: ("id" :: Text)
    profileResponseEnt <- o .: ("ent" :: Text)
    profileResponseEntId <- o .: ("ent_id" :: Text)
    profileResponseGender <- o .: ("gender" :: Text)
    profileResponseBirthdate <- o .: ("birthdate" :: Text)
    profileResponseWebsite <- o .: ("website" :: Text)
    profileResponseLocation <- o .: ("location" :: Text)
    profileResponseSignature <- o .: ("signature" :: Text)
    profileResponseDebug <- o .: ("debug" :: Text)
    profileResponseKarmaGood <- o .: ("karma_good" :: Text)
    profileResponseKarmaBad <- o .: ("karma_bad" :: Text)
    profileResponseGuard <- o .: ("guard" :: Text)
    profileResponseCreatedAt <- o .: ("created_at" :: Text)
    profileResponseModifiedAt <- o .: ("modified_at" :: Text)
    pure $ ProfileResponse {
      profileResponseId = profileResponseId,
      profileResponseEnt = profileResponseEnt,
      profileResponseEntId = profileResponseEntId,
      profileResponseGender = profileResponseGender,
      profileResponseBirthdate = profileResponseBirthdate,
      profileResponseWebsite = profileResponseWebsite,
      profileResponseLocation = profileResponseLocation,
      profileResponseSignature = profileResponseSignature,
      profileResponseDebug = profileResponseDebug,
      profileResponseKarmaGood = profileResponseKarmaGood,
      profileResponseKarmaBad = profileResponseKarmaBad,
      profileResponseGuard = profileResponseGuard,
      profileResponseCreatedAt = profileResponseCreatedAt,
      profileResponseModifiedAt = profileResponseModifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ProfileResponse where
  toJSON ProfileResponse{..} = object $
    [ "tag" .= ("ProfileResponse" :: Text)
    , "id" .= profileResponseId
    , "ent" .= profileResponseEnt
    , "ent_id" .= profileResponseEntId
    , "gender" .= profileResponseGender
    , "birthdate" .= profileResponseBirthdate
    , "website" .= profileResponseWebsite
    , "location" .= profileResponseLocation
    , "signature" .= profileResponseSignature
    , "debug" .= profileResponseDebug
    , "karma_good" .= profileResponseKarmaGood
    , "karma_bad" .= profileResponseKarmaBad
    , "guard" .= profileResponseGuard
    , "created_at" .= profileResponseCreatedAt
    , "modified_at" .= profileResponseModifiedAt
    ]


instance Eq ProfileResponse where
  (==) a b = profileResponseId a == profileResponseId b && profileResponseEnt a == profileResponseEnt b && profileResponseEntId a == profileResponseEntId b && profileResponseGender a == profileResponseGender b && profileResponseBirthdate a == profileResponseBirthdate b && profileResponseWebsite a == profileResponseWebsite b && profileResponseLocation a == profileResponseLocation b && profileResponseSignature a == profileResponseSignature b && profileResponseDebug a == profileResponseDebug b && profileResponseKarmaGood a == profileResponseKarmaGood b && profileResponseKarmaBad a == profileResponseKarmaBad b && profileResponseGuard a == profileResponseGuard b && profileResponseCreatedAt a == profileResponseCreatedAt b && profileResponseModifiedAt a == profileResponseModifiedAt b

instance Show ProfileResponse where
    show rec = "profileResponseId: " <> show (profileResponseId rec) <> ", " <> "profileResponseEnt: " <> show (profileResponseEnt rec) <> ", " <> "profileResponseEntId: " <> show (profileResponseEntId rec) <> ", " <> "profileResponseGender: " <> show (profileResponseGender rec) <> ", " <> "profileResponseBirthdate: " <> show (profileResponseBirthdate rec) <> ", " <> "profileResponseWebsite: " <> show (profileResponseWebsite rec) <> ", " <> "profileResponseLocation: " <> show (profileResponseLocation rec) <> ", " <> "profileResponseSignature: " <> show (profileResponseSignature rec) <> ", " <> "profileResponseDebug: " <> show (profileResponseDebug rec) <> ", " <> "profileResponseKarmaGood: " <> show (profileResponseKarmaGood rec) <> ", " <> "profileResponseKarmaBad: " <> show (profileResponseKarmaBad rec) <> ", " <> "profileResponseGuard: " <> show (profileResponseGuard rec) <> ", " <> "profileResponseCreatedAt: " <> show (profileResponseCreatedAt rec) <> ", " <> "profileResponseModifiedAt: " <> show (profileResponseModifiedAt rec)

data ProfileResponses = ProfileResponses {
  profileResponses :: !([ProfileResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON ProfileResponses where
  parseJSON (Object o) = do
    profileResponses <- o .: ("profile_responses" :: Text)
    pure $ ProfileResponses {
      profileResponses = profileResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ProfileResponses where
  toJSON ProfileResponses{..} = object $
    [ "tag" .= ("ProfileResponses" :: Text)
    , "profile_responses" .= profileResponses
    ]


instance Eq ProfileResponses where
  (==) a b = profileResponses a == profileResponses b

instance Show ProfileResponses where
    show rec = "profileResponses: " <> show (profileResponses rec)
-- footer