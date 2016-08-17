{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.PmIn where


import LN.T.PmIn
import LN.T.User


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

data PmInPackResponse = PmInPackResponse {
  pmInPackResponsePmIn :: !(PmInResponse),
  pmInPackResponsePmInId :: !(Int64),
  pmInPackResponseUser :: !(UserSanitizedResponse),
  pmInPackResponseUserId :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON PmInPackResponse where
  parseJSON (Object o) = do
    pmInPackResponsePmIn <- o .: ("pm_in" :: Text)
    pmInPackResponsePmInId <- o .: ("pm_in_id" :: Text)
    pmInPackResponseUser <- o .: ("user" :: Text)
    pmInPackResponseUserId <- o .: ("user_id" :: Text)
    pure $ PmInPackResponse {
      pmInPackResponsePmIn = pmInPackResponsePmIn,
      pmInPackResponsePmInId = pmInPackResponsePmInId,
      pmInPackResponseUser = pmInPackResponseUser,
      pmInPackResponseUserId = pmInPackResponseUserId
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON PmInPackResponse where
  toJSON PmInPackResponse{..} = object $
    [ "tag" .= ("PmInPackResponse" :: Text)
    , "pm_in" .= pmInPackResponsePmIn
    , "pm_in_id" .= pmInPackResponsePmInId
    , "user" .= pmInPackResponseUser
    , "user_id" .= pmInPackResponseUserId
    ]


instance Eq PmInPackResponse where
  (==) a b = pmInPackResponsePmIn a == pmInPackResponsePmIn b && pmInPackResponsePmInId a == pmInPackResponsePmInId b && pmInPackResponseUser a == pmInPackResponseUser b && pmInPackResponseUserId a == pmInPackResponseUserId b

instance Show PmInPackResponse where
    show rec = "pmInPackResponsePmIn: " <> show (pmInPackResponsePmIn rec) <> ", " <> "pmInPackResponsePmInId: " <> show (pmInPackResponsePmInId rec) <> ", " <> "pmInPackResponseUser: " <> show (pmInPackResponseUser rec) <> ", " <> "pmInPackResponseUserId: " <> show (pmInPackResponseUserId rec)

data PmInPackResponses = PmInPackResponses {
  pmInPackResponses :: !([PmInPackResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON PmInPackResponses where
  parseJSON (Object o) = do
    pmInPackResponses <- o .: ("pm_in_pack_responses" :: Text)
    pure $ PmInPackResponses {
      pmInPackResponses = pmInPackResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON PmInPackResponses where
  toJSON PmInPackResponses{..} = object $
    [ "tag" .= ("PmInPackResponses" :: Text)
    , "pm_in_pack_responses" .= pmInPackResponses
    ]


instance Eq PmInPackResponses where
  (==) a b = pmInPackResponses a == pmInPackResponses b

instance Show PmInPackResponses where
    show rec = "pmInPackResponses: " <> show (pmInPackResponses rec)
-- footer