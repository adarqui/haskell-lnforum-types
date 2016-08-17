{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.PmOut where


import LN.T.PmOut
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

data PmOutPackResponse = PmOutPackResponse {
  pmOutPackResponsePmOut :: !(PmOutResponse),
  pmOutPackResponsePmOutId :: !(Int64),
  pmOutPackResponseUser :: !(UserSanitizedResponse),
  pmOutPackResponseUserId :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON PmOutPackResponse where
  parseJSON (Object o) = do
    pmOutPackResponsePmOut <- o .: ("pm_out" :: Text)
    pmOutPackResponsePmOutId <- o .: ("pm_out_id" :: Text)
    pmOutPackResponseUser <- o .: ("user" :: Text)
    pmOutPackResponseUserId <- o .: ("user_id" :: Text)
    pure $ PmOutPackResponse {
      pmOutPackResponsePmOut = pmOutPackResponsePmOut,
      pmOutPackResponsePmOutId = pmOutPackResponsePmOutId,
      pmOutPackResponseUser = pmOutPackResponseUser,
      pmOutPackResponseUserId = pmOutPackResponseUserId
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON PmOutPackResponse where
  toJSON PmOutPackResponse{..} = object $
    [ "tag" .= ("PmOutPackResponse" :: Text)
    , "pm_out" .= pmOutPackResponsePmOut
    , "pm_out_id" .= pmOutPackResponsePmOutId
    , "user" .= pmOutPackResponseUser
    , "user_id" .= pmOutPackResponseUserId
    ]


instance Eq PmOutPackResponse where
  (==) a b = pmOutPackResponsePmOut a == pmOutPackResponsePmOut b && pmOutPackResponsePmOutId a == pmOutPackResponsePmOutId b && pmOutPackResponseUser a == pmOutPackResponseUser b && pmOutPackResponseUserId a == pmOutPackResponseUserId b

instance Show PmOutPackResponse where
    show rec = "pmOutPackResponsePmOut: " <> show (pmOutPackResponsePmOut rec) <> ", " <> "pmOutPackResponsePmOutId: " <> show (pmOutPackResponsePmOutId rec) <> ", " <> "pmOutPackResponseUser: " <> show (pmOutPackResponseUser rec) <> ", " <> "pmOutPackResponseUserId: " <> show (pmOutPackResponseUserId rec)

data PmOutPackResponses = PmOutPackResponses {
  pmOutPackResponses :: !([PmOutPackResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON PmOutPackResponses where
  parseJSON (Object o) = do
    pmOutPackResponses <- o .: ("pm_out_pack_responses" :: Text)
    pure $ PmOutPackResponses {
      pmOutPackResponses = pmOutPackResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON PmOutPackResponses where
  toJSON PmOutPackResponses{..} = object $
    [ "tag" .= ("PmOutPackResponses" :: Text)
    , "pm_out_pack_responses" .= pmOutPackResponses
    ]


instance Eq PmOutPackResponses where
  (==) a b = pmOutPackResponses a == pmOutPackResponses b

instance Show PmOutPackResponses where
    show rec = "pmOutPackResponses: " <> show (pmOutPackResponses rec)
-- footer