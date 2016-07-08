{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Substitutions where





import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Int            (Int64)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)

data Substitutions
  = SubsExpr Substitutions Substitutions
  | SubsOneOf [Text]
  | SubsAllOf [Text]
  | SubsBoth Substitutions Substitutions



instance FromJSON Substitutions where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("SubsExpr" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0, x1] -> SubsExpr <$> parseJSON x0 <*> parseJSON x1
          _ -> fail "FromJON Typemismatch: SubsExpr"

      ("SubsOneOf" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> SubsOneOf <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: SubsOneOf"

      ("SubsAllOf" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> SubsAllOf <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: SubsAllOf"

      ("SubsBoth" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0, x1] -> SubsBoth <$> parseJSON x0 <*> parseJSON x1
          _ -> fail "FromJON Typemismatch: SubsBoth"

      _ -> fail "Could not parse Substitutions"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Substitutions where
  toJSON (SubsExpr x0 x1) = object $
    [ "tag" .= ("SubsExpr" :: Text)
    , "contents" .= [toJSON x0, toJSON x1]
    ]
  toJSON (SubsOneOf x0) = object $
    [ "tag" .= ("SubsOneOf" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (SubsAllOf x0) = object $
    [ "tag" .= ("SubsAllOf" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (SubsBoth x0 x1) = object $
    [ "tag" .= ("SubsBoth" :: Text)
    , "contents" .= [toJSON x0, toJSON x1]
    ]


instance Eq Substitutions where
  (==) (SubsExpr x0a x1a) (SubsExpr x0b x1b) = x0a == x0b && x1a == x1b
  (==) (SubsOneOf x0a) (SubsOneOf x0b) = x0a == x0b
  (==) (SubsAllOf x0a) (SubsAllOf x0b) = x0a == x0b
  (==) (SubsBoth x0a x1a) (SubsBoth x0b x1b) = x0a == x0b && x1a == x1b
  (==) _ _ = False

instance Show Substitutions where
  show (SubsExpr x0 x1) = "subs_expr: " <> show x0 <> " " <> show x1
  show (SubsOneOf x0) = "subs_one_of: " <> show x0
  show (SubsAllOf x0) = "subs_all_of: " <> show x0
  show (SubsBoth x0 x1) = "subs_both: " <> show x0 <> " " <> show x1


data TySubstitutions
  = TySubsExpr 
  | TySubsOneOf 
  | TySubsAllOf 
  | TySubsBoth 



instance FromJSON TySubstitutions where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("TySubsExpr" :: Text) -> do
        pure TySubsExpr

      ("TySubsOneOf" :: Text) -> do
        pure TySubsOneOf

      ("TySubsAllOf" :: Text) -> do
        pure TySubsAllOf

      ("TySubsBoth" :: Text) -> do
        pure TySubsBoth

      _ -> fail "Could not parse TySubstitutions"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TySubstitutions where
  toJSON (TySubsExpr ) = object $
    [ "tag" .= ("TySubsExpr" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TySubsOneOf ) = object $
    [ "tag" .= ("TySubsOneOf" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TySubsAllOf ) = object $
    [ "tag" .= ("TySubsAllOf" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TySubsBoth ) = object $
    [ "tag" .= ("TySubsBoth" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq TySubstitutions where
  (==) TySubsExpr TySubsExpr = True
  (==) TySubsOneOf TySubsOneOf = True
  (==) TySubsAllOf TySubsAllOf = True
  (==) TySubsBoth TySubsBoth = True
  (==) _ _ = False

instance Show TySubstitutions where
  show TySubsExpr = "ty_subs_expr"
  show TySubsOneOf = "ty_subs_one_of"
  show TySubsAllOf = "ty_subs_all_of"
  show TySubsBoth = "ty_subs_both"

-- footer