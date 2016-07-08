{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Splits where





import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Int            (Int64)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)
import           Data.Default

data Splits
  = SplitAt Char Text Text
  | SplitNone 



instance FromJSON Splits where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("SplitAt" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0, x1, x2] -> SplitAt <$> parseJSON x0 <*> parseJSON x1 <*> parseJSON x2
          _ -> fail "FromJON Typemismatch: SplitAt"

      ("SplitNone" :: Text) -> do
        pure SplitNone

      _ -> fail "Could not parse Splits"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Splits where
  toJSON (SplitAt x0 x1 x2) = object $
    [ "tag" .= ("SplitAt" :: Text)
    , "contents" .= [toJSON x0, toJSON x1, toJSON x2]
    ]
  toJSON (SplitNone ) = object $
    [ "tag" .= ("SplitNone" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq Splits where
  (==) (SplitAt x0a x1a x2a) (SplitAt x0b x1b x2b) = x0a == x0b && x1a == x1b && x2a == x2b
  (==) SplitNone SplitNone = True
  (==) _ _ = False

instance Show Splits where
  show (SplitAt x0 x1 x2) = "split_at: " <> show x0 <> " " <> show x1 <> " " <> show x2
  show SplitNone = "split_none"


data TySplits
  = TySplitA 
  | TySplitNone 



instance FromJSON TySplits where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("TySplitA" :: Text) -> do
        pure TySplitA

      ("TySplitNone" :: Text) -> do
        pure TySplitNone

      _ -> fail "Could not parse TySplits"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TySplits where
  toJSON (TySplitA ) = object $
    [ "tag" .= ("TySplitA" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TySplitNone ) = object $
    [ "tag" .= ("TySplitNone" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq TySplits where
  (==) TySplitA TySplitA = True
  (==) TySplitNone TySplitNone = True
  (==) _ _ = False

instance Show TySplits where
  show TySplitA = "ty_split_a"
  show TySplitNone = "ty_split_none"

-- footer