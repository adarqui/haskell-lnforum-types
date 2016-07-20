{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Visibility where





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

data Visibility
  = Public 
  | Private 
  deriving (Generic,Typeable,NFData)


instance FromJSON Visibility where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("Public" :: Text) -> do
        pure Public

      ("Private" :: Text) -> do
        pure Private

      _ -> fail "Could not parse Visibility"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Visibility where
  toJSON (Public ) = object $
    [ "tag" .= ("Public" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Private ) = object $
    [ "tag" .= ("Private" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq Visibility where
  (==) Public Public = True
  (==) Private Private = True
  (==) _ _ = False

instance Show Visibility where
  show Public = "public"
  show Private = "private"


instance Read Visibility where
  readsPrec _ "public" = [(Public, "")]
  readsPrec _ "private" = [(Private, "")]
  readsPrec _ _ = []

-- footer