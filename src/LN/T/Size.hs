{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Size where





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

data Size
  = XSmall 
  | Small 
  | Medium 
  | Large 
  | XLarge 
  deriving (Generic,Typeable,NFData)


instance FromJSON Size where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("XSmall" :: Text) -> do
        pure XSmall

      ("Small" :: Text) -> do
        pure Small

      ("Medium" :: Text) -> do
        pure Medium

      ("Large" :: Text) -> do
        pure Large

      ("XLarge" :: Text) -> do
        pure XLarge

      _ -> fail "Could not parse Size"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Size where
  toJSON (XSmall ) = object $
    [ "tag" .= ("XSmall" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Small ) = object $
    [ "tag" .= ("Small" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Medium ) = object $
    [ "tag" .= ("Medium" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Large ) = object $
    [ "tag" .= ("Large" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (XLarge ) = object $
    [ "tag" .= ("XLarge" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq Size where
  (==) XSmall XSmall = True
  (==) Small Small = True
  (==) Medium Medium = True
  (==) Large Large = True
  (==) XLarge XLarge = True
  (==) _ _ = False

instance Show Size where
  show XSmall = "xsmall"
  show Small = "small"
  show Medium = "medium"
  show Large = "large"
  show XLarge = "xlarge"

-- footer