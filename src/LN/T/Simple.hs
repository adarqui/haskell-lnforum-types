{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Simple where





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

data SimpleIntRequest = SimpleIntRequest {
  simpleIntRequest :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON SimpleIntRequest where
  parseJSON (Object o) = do
    simpleIntRequest <- o .: ("simple_int_request" :: Text)
    pure $ SimpleIntRequest {
      simpleIntRequest = simpleIntRequest
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON SimpleIntRequest where
  toJSON SimpleIntRequest{..} = object $
    [ "tag" .= ("SimpleIntRequest" :: Text)
    , "simple_int_request" .= simpleIntRequest
    ]


instance Eq SimpleIntRequest where
  (==) a b = simpleIntRequest a == simpleIntRequest b

instance Show SimpleIntRequest where
    show rec = "simpleIntRequest: " <> show (simpleIntRequest rec)

data SimpleIntResponse = SimpleIntResponse {
  simpleIntResponse :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON SimpleIntResponse where
  parseJSON (Object o) = do
    simpleIntResponse <- o .: ("simple_int_response" :: Text)
    pure $ SimpleIntResponse {
      simpleIntResponse = simpleIntResponse
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON SimpleIntResponse where
  toJSON SimpleIntResponse{..} = object $
    [ "tag" .= ("SimpleIntResponse" :: Text)
    , "simple_int_response" .= simpleIntResponse
    ]


instance Eq SimpleIntResponse where
  (==) a b = simpleIntResponse a == simpleIntResponse b

instance Show SimpleIntResponse where
    show rec = "simpleIntResponse: " <> show (simpleIntResponse rec)

data SimpleIntsRequest = SimpleIntsRequest {
  simpleIntsRequest :: !([Int64])
}  deriving (Generic,Typeable,NFData)


instance FromJSON SimpleIntsRequest where
  parseJSON (Object o) = do
    simpleIntsRequest <- o .: ("simple_ints_request" :: Text)
    pure $ SimpleIntsRequest {
      simpleIntsRequest = simpleIntsRequest
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON SimpleIntsRequest where
  toJSON SimpleIntsRequest{..} = object $
    [ "tag" .= ("SimpleIntsRequest" :: Text)
    , "simple_ints_request" .= simpleIntsRequest
    ]


instance Eq SimpleIntsRequest where
  (==) a b = simpleIntsRequest a == simpleIntsRequest b

instance Show SimpleIntsRequest where
    show rec = "simpleIntsRequest: " <> show (simpleIntsRequest rec)

data SimpleIntsResponse = SimpleIntsResponse {
  simpleIntsResponse :: !([Int64])
}  deriving (Generic,Typeable,NFData)


instance FromJSON SimpleIntsResponse where
  parseJSON (Object o) = do
    simpleIntsResponse <- o .: ("simple_ints_response" :: Text)
    pure $ SimpleIntsResponse {
      simpleIntsResponse = simpleIntsResponse
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON SimpleIntsResponse where
  toJSON SimpleIntsResponse{..} = object $
    [ "tag" .= ("SimpleIntsResponse" :: Text)
    , "simple_ints_response" .= simpleIntsResponse
    ]


instance Eq SimpleIntsResponse where
  (==) a b = simpleIntsResponse a == simpleIntsResponse b

instance Show SimpleIntsResponse where
    show rec = "simpleIntsResponse: " <> show (simpleIntsResponse rec)

data SimpleStringRequest = SimpleStringRequest {
  simpleStringRequest :: !(Text)
}  deriving (Generic,Typeable,NFData)


instance FromJSON SimpleStringRequest where
  parseJSON (Object o) = do
    simpleStringRequest <- o .: ("simple_string_request" :: Text)
    pure $ SimpleStringRequest {
      simpleStringRequest = simpleStringRequest
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON SimpleStringRequest where
  toJSON SimpleStringRequest{..} = object $
    [ "tag" .= ("SimpleStringRequest" :: Text)
    , "simple_string_request" .= simpleStringRequest
    ]


instance Eq SimpleStringRequest where
  (==) a b = simpleStringRequest a == simpleStringRequest b

instance Show SimpleStringRequest where
    show rec = "simpleStringRequest: " <> show (simpleStringRequest rec)

data SimpleStringResponse = SimpleStringResponse {
  simpleStringResponse :: !(Text)
}  deriving (Generic,Typeable,NFData)


instance FromJSON SimpleStringResponse where
  parseJSON (Object o) = do
    simpleStringResponse <- o .: ("simple_string_response" :: Text)
    pure $ SimpleStringResponse {
      simpleStringResponse = simpleStringResponse
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON SimpleStringResponse where
  toJSON SimpleStringResponse{..} = object $
    [ "tag" .= ("SimpleStringResponse" :: Text)
    , "simple_string_response" .= simpleStringResponse
    ]


instance Eq SimpleStringResponse where
  (==) a b = simpleStringResponse a == simpleStringResponse b

instance Show SimpleStringResponse where
    show rec = "simpleStringResponse: " <> show (simpleStringResponse rec)

data SimpleStringsRequest = SimpleStringsRequest {
  simpleStringsRequest :: !([Text])
}  deriving (Generic,Typeable,NFData)


instance FromJSON SimpleStringsRequest where
  parseJSON (Object o) = do
    simpleStringsRequest <- o .: ("simple_strings_request" :: Text)
    pure $ SimpleStringsRequest {
      simpleStringsRequest = simpleStringsRequest
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON SimpleStringsRequest where
  toJSON SimpleStringsRequest{..} = object $
    [ "tag" .= ("SimpleStringsRequest" :: Text)
    , "simple_strings_request" .= simpleStringsRequest
    ]


instance Eq SimpleStringsRequest where
  (==) a b = simpleStringsRequest a == simpleStringsRequest b

instance Show SimpleStringsRequest where
    show rec = "simpleStringsRequest: " <> show (simpleStringsRequest rec)

data SimpleStringsResponse = SimpleStringsResponse {
  simpleStringsResponse :: !([Text])
}  deriving (Generic,Typeable,NFData)


instance FromJSON SimpleStringsResponse where
  parseJSON (Object o) = do
    simpleStringsResponse <- o .: ("simple_strings_response" :: Text)
    pure $ SimpleStringsResponse {
      simpleStringsResponse = simpleStringsResponse
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON SimpleStringsResponse where
  toJSON SimpleStringsResponse{..} = object $
    [ "tag" .= ("SimpleStringsResponse" :: Text)
    , "simple_strings_response" .= simpleStringsResponse
    ]


instance Eq SimpleStringsResponse where
  (==) a b = simpleStringsResponse a == simpleStringsResponse b

instance Show SimpleStringsResponse where
    show rec = "simpleStringsResponse: " <> show (simpleStringsResponse rec)
-- footer