{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Error where





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

data ApplicationError
  = Error_Unknown 
  | Error_NotFound 
  | Error_PermissionDenied 
  | Error_AlreadyExists 
  | Error_Visibility 
  | Error_Membership 
  | Error_Validation ValidationError
  | Error_NotImplemented 
  | Error_InvalidArguments Text
  | Error_Unexpected 
  deriving (Generic,Typeable,NFData)


instance FromJSON ApplicationError where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("Error_Unknown" :: Text) -> do
        pure Error_Unknown

      ("Error_NotFound" :: Text) -> do
        pure Error_NotFound

      ("Error_PermissionDenied" :: Text) -> do
        pure Error_PermissionDenied

      ("Error_AlreadyExists" :: Text) -> do
        pure Error_AlreadyExists

      ("Error_Visibility" :: Text) -> do
        pure Error_Visibility

      ("Error_Membership" :: Text) -> do
        pure Error_Membership

      ("Error_Validation" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> Error_Validation <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: Error_Validation"

      ("Error_NotImplemented" :: Text) -> do
        pure Error_NotImplemented

      ("Error_InvalidArguments" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> Error_InvalidArguments <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: Error_InvalidArguments"

      ("Error_Unexpected" :: Text) -> do
        pure Error_Unexpected

      _ -> fail "Could not parse ApplicationError"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ApplicationError where
  toJSON (Error_Unknown ) = object $
    [ "tag" .= ("Error_Unknown" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Error_NotFound ) = object $
    [ "tag" .= ("Error_NotFound" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Error_PermissionDenied ) = object $
    [ "tag" .= ("Error_PermissionDenied" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Error_AlreadyExists ) = object $
    [ "tag" .= ("Error_AlreadyExists" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Error_Visibility ) = object $
    [ "tag" .= ("Error_Visibility" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Error_Membership ) = object $
    [ "tag" .= ("Error_Membership" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Error_Validation x0) = object $
    [ "tag" .= ("Error_Validation" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (Error_NotImplemented ) = object $
    [ "tag" .= ("Error_NotImplemented" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Error_InvalidArguments x0) = object $
    [ "tag" .= ("Error_InvalidArguments" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (Error_Unexpected ) = object $
    [ "tag" .= ("Error_Unexpected" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq ApplicationError where
  (==) Error_Unknown Error_Unknown = True
  (==) Error_NotFound Error_NotFound = True
  (==) Error_PermissionDenied Error_PermissionDenied = True
  (==) Error_AlreadyExists Error_AlreadyExists = True
  (==) Error_Visibility Error_Visibility = True
  (==) Error_Membership Error_Membership = True
  (==) (Error_Validation x0a) (Error_Validation x0b) = x0a == x0b
  (==) Error_NotImplemented Error_NotImplemented = True
  (==) (Error_InvalidArguments x0a) (Error_InvalidArguments x0b) = x0a == x0b
  (==) Error_Unexpected Error_Unexpected = True
  (==) _ _ = False

instance Show ApplicationError where
  show Error_Unknown = "error_unknown"
  show Error_NotFound = "error_not_found"
  show Error_PermissionDenied = "error_permission_denied"
  show Error_AlreadyExists = "error_already_exists"
  show Error_Visibility = "error_visibility"
  show Error_Membership = "error_membership"
  show (Error_Validation x0) = "error_validation: " <> show x0
  show Error_NotImplemented = "error_not_implemented"
  show (Error_InvalidArguments x0) = "error_invalid_arguments: " <> show x0
  show Error_Unexpected = "error_unexpected"


instance Default ApplicationError where
  def = Error_Unknown

data ValidationError
  = Validate ValidationErrorCode (Maybe Text)
  deriving (Generic,Typeable,NFData)


instance FromJSON ValidationError where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("Validate" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0, x1] -> Validate <$> parseJSON x0 <*> parseJSON x1
          _ -> fail "FromJON Typemismatch: Validate"

      _ -> fail "Could not parse ValidationError"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ValidationError where
  toJSON (Validate x0 x1) = object $
    [ "tag" .= ("Validate" :: Text)
    , "contents" .= [toJSON x0, toJSON x1]
    ]


instance Eq ValidationError where
  (==) (Validate x0a x1a) (Validate x0b x1b) = x0a == x0b && x1a == x1b


instance Show ValidationError where
  show (Validate x0 x1) = "validate: " <> show x0 <> " " <> show x1


instance Default ValidationError where
  def = Validate Validate_Unknown Nothing

data ValidationErrorCode
  = Validate_Unknown 
  | Validate_InvalidCharacters 
  | Validate_InvalidEmail 
  | Validate_InvalidDate 
  | Validate_CannotBeEmpty 
  | Validate_TooLong 
  | Validate_TooShort 
  | Validate_GreaterThanMaximum 
  | Validate_SmallerThanMinimum 
  | Validate_Reason Text
  deriving (Generic,Typeable,NFData)


instance FromJSON ValidationErrorCode where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("Validate_Unknown" :: Text) -> do
        pure Validate_Unknown

      ("Validate_InvalidCharacters" :: Text) -> do
        pure Validate_InvalidCharacters

      ("Validate_InvalidEmail" :: Text) -> do
        pure Validate_InvalidEmail

      ("Validate_InvalidDate" :: Text) -> do
        pure Validate_InvalidDate

      ("Validate_CannotBeEmpty" :: Text) -> do
        pure Validate_CannotBeEmpty

      ("Validate_TooLong" :: Text) -> do
        pure Validate_TooLong

      ("Validate_TooShort" :: Text) -> do
        pure Validate_TooShort

      ("Validate_GreaterThanMaximum" :: Text) -> do
        pure Validate_GreaterThanMaximum

      ("Validate_SmallerThanMinimum" :: Text) -> do
        pure Validate_SmallerThanMinimum

      ("Validate_Reason" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> Validate_Reason <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: Validate_Reason"

      _ -> fail "Could not parse ValidationErrorCode"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ValidationErrorCode where
  toJSON (Validate_Unknown ) = object $
    [ "tag" .= ("Validate_Unknown" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Validate_InvalidCharacters ) = object $
    [ "tag" .= ("Validate_InvalidCharacters" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Validate_InvalidEmail ) = object $
    [ "tag" .= ("Validate_InvalidEmail" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Validate_InvalidDate ) = object $
    [ "tag" .= ("Validate_InvalidDate" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Validate_CannotBeEmpty ) = object $
    [ "tag" .= ("Validate_CannotBeEmpty" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Validate_TooLong ) = object $
    [ "tag" .= ("Validate_TooLong" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Validate_TooShort ) = object $
    [ "tag" .= ("Validate_TooShort" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Validate_GreaterThanMaximum ) = object $
    [ "tag" .= ("Validate_GreaterThanMaximum" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Validate_SmallerThanMinimum ) = object $
    [ "tag" .= ("Validate_SmallerThanMinimum" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Validate_Reason x0) = object $
    [ "tag" .= ("Validate_Reason" :: Text)
    , "contents" .= [toJSON x0]
    ]


instance Eq ValidationErrorCode where
  (==) Validate_Unknown Validate_Unknown = True
  (==) Validate_InvalidCharacters Validate_InvalidCharacters = True
  (==) Validate_InvalidEmail Validate_InvalidEmail = True
  (==) Validate_InvalidDate Validate_InvalidDate = True
  (==) Validate_CannotBeEmpty Validate_CannotBeEmpty = True
  (==) Validate_TooLong Validate_TooLong = True
  (==) Validate_TooShort Validate_TooShort = True
  (==) Validate_GreaterThanMaximum Validate_GreaterThanMaximum = True
  (==) Validate_SmallerThanMinimum Validate_SmallerThanMinimum = True
  (==) (Validate_Reason x0a) (Validate_Reason x0b) = x0a == x0b
  (==) _ _ = False

instance Show ValidationErrorCode where
  show Validate_Unknown = "validate_unknown"
  show Validate_InvalidCharacters = "validate_invalid_characters"
  show Validate_InvalidEmail = "validate_invalid_email"
  show Validate_InvalidDate = "validate_invalid_date"
  show Validate_CannotBeEmpty = "validate_cannot_be_empty"
  show Validate_TooLong = "validate_too_long"
  show Validate_TooShort = "validate_too_short"
  show Validate_GreaterThanMaximum = "validate_greater_than_maximum"
  show Validate_SmallerThanMinimum = "validate_smaller_than_minimum"
  show (Validate_Reason x0) = "validate_reason: " <> show x0


instance Default ValidationErrorCode where
  def = Validate_Unknown
-- footer