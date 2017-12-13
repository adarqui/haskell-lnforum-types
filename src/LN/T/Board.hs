{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Board where


import LN.T.DepList
import LN.T.Visibility


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

data BoardType
  = ISBN13 !(Text)
  | ISBN10 !(Text)
  | ISBN !(Text)
  | URL !(Text)
  | SourceNone 
  deriving (Generic,Typeable,NFData)


instance FromJSON BoardType where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("ISBN13" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ISBN13 <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ISBN13"

      ("ISBN10" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ISBN10 <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ISBN10"

      ("ISBN" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ISBN <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ISBN"

      ("URL" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> URL <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: URL"

      ("SourceNone" :: Text) -> do
        pure SourceNone

      _ -> fail "Could not parse BoardType"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BoardType where
  toJSON (ISBN13 x0) = object $
    [ "tag" .= ("ISBN13" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ISBN10 x0) = object $
    [ "tag" .= ("ISBN10" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ISBN x0) = object $
    [ "tag" .= ("ISBN" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (URL x0) = object $
    [ "tag" .= ("URL" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (SourceNone ) = object $
    [ "tag" .= ("SourceNone" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq BoardType where
  (==) (ISBN13 x0a) (ISBN13 x0b) = x0a == x0b
  (==) (ISBN10 x0a) (ISBN10 x0b) = x0a == x0b
  (==) (ISBN x0a) (ISBN x0b) = x0a == x0b
  (==) (URL x0a) (URL x0b) = x0a == x0b
  (==) SourceNone SourceNone = True
  (==) _ _ = False

instance Show BoardType where
  show (ISBN13 x0) = "isbn13: " <> show x0
  show (ISBN10 x0) = "isbn10: " <> show x0
  show (ISBN x0) = "isbn: " <> show x0
  show (URL x0) = "url: " <> show x0
  show SourceNone = "source_none"


data TyBoardType
  = TyISBN13 
  | TyISBN10 
  | TyISBN 
  | TyURL 
  | TySourceNone 
  deriving (Generic,Typeable,NFData)


instance FromJSON TyBoardType where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("TyISBN13" :: Text) -> do
        pure TyISBN13

      ("TyISBN10" :: Text) -> do
        pure TyISBN10

      ("TyISBN" :: Text) -> do
        pure TyISBN

      ("TyURL" :: Text) -> do
        pure TyURL

      ("TySourceNone" :: Text) -> do
        pure TySourceNone

      _ -> fail "Could not parse TyBoardType"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TyBoardType where
  toJSON (TyISBN13 ) = object $
    [ "tag" .= ("TyISBN13" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyISBN10 ) = object $
    [ "tag" .= ("TyISBN10" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyISBN ) = object $
    [ "tag" .= ("TyISBN" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyURL ) = object $
    [ "tag" .= ("TyURL" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TySourceNone ) = object $
    [ "tag" .= ("TySourceNone" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq TyBoardType where
  (==) TyISBN13 TyISBN13 = True
  (==) TyISBN10 TyISBN10 = True
  (==) TyISBN TyISBN = True
  (==) TyURL TyURL = True
  (==) TySourceNone TySourceNone = True
  (==) _ _ = False

instance Show TyBoardType where
  show TyISBN13 = "ty_isbn13"
  show TyISBN10 = "ty_isbn10"
  show TyISBN = "ty_isbn"
  show TyURL = "ty_url"
  show TySourceNone = "ty_source_none"


data BoardRequest = BoardRequest {
  boardRequestDisplayName :: !(Text),
  boardRequestDescription :: !(Text),
  boardRequestSource :: !(BoardType),
  boardRequestAuthor :: !((Maybe [Text])),
  boardRequestPrerequisites :: !([Text]),
  boardRequestCategories :: !([Text]),
  boardRequestVisibility :: !(Visibility),
  boardRequestCounter :: !(Int),
  boardRequestVersion :: !((Maybe Text)),
  boardRequestUrls :: !((Maybe [Text])),
  boardRequestIcon :: !((Maybe Text)),
  boardRequestTags :: !([Text]),
  boardRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardRequest where
  parseJSON (Object o) = do
    boardRequestDisplayName <- o .: ("display_name" :: Text)
    boardRequestDescription <- o .: ("description" :: Text)
    boardRequestSource <- o .: ("source" :: Text)
    boardRequestAuthor <- o .: ("author" :: Text)
    boardRequestPrerequisites <- o .: ("prerequisites" :: Text)
    boardRequestCategories <- o .: ("categories" :: Text)
    boardRequestVisibility <- o .: ("visibility" :: Text)
    boardRequestCounter <- o .: ("counter" :: Text)
    boardRequestVersion <- o .: ("version" :: Text)
    boardRequestUrls <- o .: ("urls" :: Text)
    boardRequestIcon <- o .: ("icon" :: Text)
    boardRequestTags <- o .: ("tags" :: Text)
    boardRequestGuard <- o .: ("guard" :: Text)
    pure $ BoardRequest {
      boardRequestDisplayName = boardRequestDisplayName,
      boardRequestDescription = boardRequestDescription,
      boardRequestSource = boardRequestSource,
      boardRequestAuthor = boardRequestAuthor,
      boardRequestPrerequisites = boardRequestPrerequisites,
      boardRequestCategories = boardRequestCategories,
      boardRequestVisibility = boardRequestVisibility,
      boardRequestCounter = boardRequestCounter,
      boardRequestVersion = boardRequestVersion,
      boardRequestUrls = boardRequestUrls,
      boardRequestIcon = boardRequestIcon,
      boardRequestTags = boardRequestTags,
      boardRequestGuard = boardRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BoardRequest where
  toJSON BoardRequest{..} = object $
    [ "tag" .= ("BoardRequest" :: Text)
    , "display_name" .= boardRequestDisplayName
    , "description" .= boardRequestDescription
    , "source" .= boardRequestSource
    , "author" .= boardRequestAuthor
    , "prerequisites" .= boardRequestPrerequisites
    , "categories" .= boardRequestCategories
    , "visibility" .= boardRequestVisibility
    , "counter" .= boardRequestCounter
    , "version" .= boardRequestVersion
    , "urls" .= boardRequestUrls
    , "icon" .= boardRequestIcon
    , "tags" .= boardRequestTags
    , "guard" .= boardRequestGuard
    ]


instance Eq BoardRequest where
  (==) a b = boardRequestDisplayName a == boardRequestDisplayName b && boardRequestDescription a == boardRequestDescription b && boardRequestSource a == boardRequestSource b && boardRequestAuthor a == boardRequestAuthor b && boardRequestPrerequisites a == boardRequestPrerequisites b && boardRequestCategories a == boardRequestCategories b && boardRequestVisibility a == boardRequestVisibility b && boardRequestCounter a == boardRequestCounter b && boardRequestVersion a == boardRequestVersion b && boardRequestUrls a == boardRequestUrls b && boardRequestIcon a == boardRequestIcon b && boardRequestTags a == boardRequestTags b && boardRequestGuard a == boardRequestGuard b

instance Show BoardRequest where
    show rec = "boardRequestDisplayName: " <> show (boardRequestDisplayName rec) <> ", " <> "boardRequestDescription: " <> show (boardRequestDescription rec) <> ", " <> "boardRequestSource: " <> show (boardRequestSource rec) <> ", " <> "boardRequestAuthor: " <> show (boardRequestAuthor rec) <> ", " <> "boardRequestPrerequisites: " <> show (boardRequestPrerequisites rec) <> ", " <> "boardRequestCategories: " <> show (boardRequestCategories rec) <> ", " <> "boardRequestVisibility: " <> show (boardRequestVisibility rec) <> ", " <> "boardRequestCounter: " <> show (boardRequestCounter rec) <> ", " <> "boardRequestVersion: " <> show (boardRequestVersion rec) <> ", " <> "boardRequestUrls: " <> show (boardRequestUrls rec) <> ", " <> "boardRequestIcon: " <> show (boardRequestIcon rec) <> ", " <> "boardRequestTags: " <> show (boardRequestTags rec) <> ", " <> "boardRequestGuard: " <> show (boardRequestGuard rec)

data BoardResponse = BoardResponse {
  boardResponseId :: !(Int64),
  boardResponseUserId :: !(Int64),
  boardResponseName :: !(Text),
  boardResponseDisplayName :: !(Text),
  boardResponseDescription :: !(Text),
  boardResponseSource :: !(BoardType),
  boardResponseAuthor :: !((Maybe [Text])),
  boardResponsePrerequisites :: !([Text]),
  boardResponseCategories :: !([Text]),
  boardResponseVisibility :: !(Visibility),
  boardResponseCounter :: !(Int),
  boardResponseVersion :: !((Maybe Text)),
  boardResponseUrls :: !((Maybe [Text])),
  boardResponseIcon :: !((Maybe Text)),
  boardResponseTags :: !([Text]),
  boardResponseActive :: !(Bool),
  boardResponseGuard :: !(Int),
  boardResponseCreatedAt :: !((Maybe UTCTime)),
  boardResponseModifiedAt :: !((Maybe UTCTime)),
  boardResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardResponse where
  parseJSON (Object o) = do
    boardResponseId <- o .: ("id" :: Text)
    boardResponseUserId <- o .: ("user_id" :: Text)
    boardResponseName <- o .: ("name" :: Text)
    boardResponseDisplayName <- o .: ("display_name" :: Text)
    boardResponseDescription <- o .: ("description" :: Text)
    boardResponseSource <- o .: ("source" :: Text)
    boardResponseAuthor <- o .: ("author" :: Text)
    boardResponsePrerequisites <- o .: ("prerequisites" :: Text)
    boardResponseCategories <- o .: ("categories" :: Text)
    boardResponseVisibility <- o .: ("visibility" :: Text)
    boardResponseCounter <- o .: ("counter" :: Text)
    boardResponseVersion <- o .: ("version" :: Text)
    boardResponseUrls <- o .: ("urls" :: Text)
    boardResponseIcon <- o .: ("icon" :: Text)
    boardResponseTags <- o .: ("tags" :: Text)
    boardResponseActive <- o .: ("active" :: Text)
    boardResponseGuard <- o .: ("guard" :: Text)
    boardResponseCreatedAt <- o .: ("created_at" :: Text)
    boardResponseModifiedAt <- o .: ("modified_at" :: Text)
    boardResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ BoardResponse {
      boardResponseId = boardResponseId,
      boardResponseUserId = boardResponseUserId,
      boardResponseName = boardResponseName,
      boardResponseDisplayName = boardResponseDisplayName,
      boardResponseDescription = boardResponseDescription,
      boardResponseSource = boardResponseSource,
      boardResponseAuthor = boardResponseAuthor,
      boardResponsePrerequisites = boardResponsePrerequisites,
      boardResponseCategories = boardResponseCategories,
      boardResponseVisibility = boardResponseVisibility,
      boardResponseCounter = boardResponseCounter,
      boardResponseVersion = boardResponseVersion,
      boardResponseUrls = boardResponseUrls,
      boardResponseIcon = boardResponseIcon,
      boardResponseTags = boardResponseTags,
      boardResponseActive = boardResponseActive,
      boardResponseGuard = boardResponseGuard,
      boardResponseCreatedAt = boardResponseCreatedAt,
      boardResponseModifiedAt = boardResponseModifiedAt,
      boardResponseActivityAt = boardResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BoardResponse where
  toJSON BoardResponse{..} = object $
    [ "tag" .= ("BoardResponse" :: Text)
    , "id" .= boardResponseId
    , "user_id" .= boardResponseUserId
    , "name" .= boardResponseName
    , "display_name" .= boardResponseDisplayName
    , "description" .= boardResponseDescription
    , "source" .= boardResponseSource
    , "author" .= boardResponseAuthor
    , "prerequisites" .= boardResponsePrerequisites
    , "categories" .= boardResponseCategories
    , "visibility" .= boardResponseVisibility
    , "counter" .= boardResponseCounter
    , "version" .= boardResponseVersion
    , "urls" .= boardResponseUrls
    , "icon" .= boardResponseIcon
    , "tags" .= boardResponseTags
    , "active" .= boardResponseActive
    , "guard" .= boardResponseGuard
    , "created_at" .= boardResponseCreatedAt
    , "modified_at" .= boardResponseModifiedAt
    , "activity_at" .= boardResponseActivityAt
    ]


instance Eq BoardResponse where
  (==) a b = boardResponseId a == boardResponseId b && boardResponseUserId a == boardResponseUserId b && boardResponseName a == boardResponseName b && boardResponseDisplayName a == boardResponseDisplayName b && boardResponseDescription a == boardResponseDescription b && boardResponseSource a == boardResponseSource b && boardResponseAuthor a == boardResponseAuthor b && boardResponsePrerequisites a == boardResponsePrerequisites b && boardResponseCategories a == boardResponseCategories b && boardResponseVisibility a == boardResponseVisibility b && boardResponseCounter a == boardResponseCounter b && boardResponseVersion a == boardResponseVersion b && boardResponseUrls a == boardResponseUrls b && boardResponseIcon a == boardResponseIcon b && boardResponseTags a == boardResponseTags b && boardResponseActive a == boardResponseActive b && boardResponseGuard a == boardResponseGuard b && boardResponseCreatedAt a == boardResponseCreatedAt b && boardResponseModifiedAt a == boardResponseModifiedAt b && boardResponseActivityAt a == boardResponseActivityAt b

instance Show BoardResponse where
    show rec = "boardResponseId: " <> show (boardResponseId rec) <> ", " <> "boardResponseUserId: " <> show (boardResponseUserId rec) <> ", " <> "boardResponseName: " <> show (boardResponseName rec) <> ", " <> "boardResponseDisplayName: " <> show (boardResponseDisplayName rec) <> ", " <> "boardResponseDescription: " <> show (boardResponseDescription rec) <> ", " <> "boardResponseSource: " <> show (boardResponseSource rec) <> ", " <> "boardResponseAuthor: " <> show (boardResponseAuthor rec) <> ", " <> "boardResponsePrerequisites: " <> show (boardResponsePrerequisites rec) <> ", " <> "boardResponseCategories: " <> show (boardResponseCategories rec) <> ", " <> "boardResponseVisibility: " <> show (boardResponseVisibility rec) <> ", " <> "boardResponseCounter: " <> show (boardResponseCounter rec) <> ", " <> "boardResponseVersion: " <> show (boardResponseVersion rec) <> ", " <> "boardResponseUrls: " <> show (boardResponseUrls rec) <> ", " <> "boardResponseIcon: " <> show (boardResponseIcon rec) <> ", " <> "boardResponseTags: " <> show (boardResponseTags rec) <> ", " <> "boardResponseActive: " <> show (boardResponseActive rec) <> ", " <> "boardResponseGuard: " <> show (boardResponseGuard rec) <> ", " <> "boardResponseCreatedAt: " <> show (boardResponseCreatedAt rec) <> ", " <> "boardResponseModifiedAt: " <> show (boardResponseModifiedAt rec) <> ", " <> "boardResponseActivityAt: " <> show (boardResponseActivityAt rec)

data BoardResponses = BoardResponses {
  boardResponses :: !([BoardResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardResponses where
  parseJSON (Object o) = do
    boardResponses <- o .: ("board_responses" :: Text)
    pure $ BoardResponses {
      boardResponses = boardResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BoardResponses where
  toJSON BoardResponses{..} = object $
    [ "tag" .= ("BoardResponses" :: Text)
    , "board_responses" .= boardResponses
    ]


instance Eq BoardResponses where
  (==) a b = boardResponses a == boardResponses b

instance Show BoardResponses where
    show rec = "boardResponses: " <> show (boardResponses rec)

data BoardStatResponse = BoardStatResponse {
  boardStatResponseBoardId :: !(Int64),
  boardStatResponseLeurons :: !(Int64),
  boardStatResponseLikes :: !(Int64),
  boardStatResponseNeutral :: !(Int64),
  boardStatResponseDislikes :: !(Int64),
  boardStatResponseStars :: !(Int64),
  boardStatResponseViews :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardStatResponse where
  parseJSON (Object o) = do
    boardStatResponseBoardId <- o .: ("board_id" :: Text)
    boardStatResponseLeurons <- o .: ("leurons" :: Text)
    boardStatResponseLikes <- o .: ("likes" :: Text)
    boardStatResponseNeutral <- o .: ("neutral" :: Text)
    boardStatResponseDislikes <- o .: ("dislikes" :: Text)
    boardStatResponseStars <- o .: ("stars" :: Text)
    boardStatResponseViews <- o .: ("views" :: Text)
    pure $ BoardStatResponse {
      boardStatResponseBoardId = boardStatResponseBoardId,
      boardStatResponseLeurons = boardStatResponseLeurons,
      boardStatResponseLikes = boardStatResponseLikes,
      boardStatResponseNeutral = boardStatResponseNeutral,
      boardStatResponseDislikes = boardStatResponseDislikes,
      boardStatResponseStars = boardStatResponseStars,
      boardStatResponseViews = boardStatResponseViews
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BoardStatResponse where
  toJSON BoardStatResponse{..} = object $
    [ "tag" .= ("BoardStatResponse" :: Text)
    , "board_id" .= boardStatResponseBoardId
    , "leurons" .= boardStatResponseLeurons
    , "likes" .= boardStatResponseLikes
    , "neutral" .= boardStatResponseNeutral
    , "dislikes" .= boardStatResponseDislikes
    , "stars" .= boardStatResponseStars
    , "views" .= boardStatResponseViews
    ]


instance Eq BoardStatResponse where
  (==) a b = boardStatResponseBoardId a == boardStatResponseBoardId b && boardStatResponseLeurons a == boardStatResponseLeurons b && boardStatResponseLikes a == boardStatResponseLikes b && boardStatResponseNeutral a == boardStatResponseNeutral b && boardStatResponseDislikes a == boardStatResponseDislikes b && boardStatResponseStars a == boardStatResponseStars b && boardStatResponseViews a == boardStatResponseViews b

instance Show BoardStatResponse where
    show rec = "boardStatResponseBoardId: " <> show (boardStatResponseBoardId rec) <> ", " <> "boardStatResponseLeurons: " <> show (boardStatResponseLeurons rec) <> ", " <> "boardStatResponseLikes: " <> show (boardStatResponseLikes rec) <> ", " <> "boardStatResponseNeutral: " <> show (boardStatResponseNeutral rec) <> ", " <> "boardStatResponseDislikes: " <> show (boardStatResponseDislikes rec) <> ", " <> "boardStatResponseStars: " <> show (boardStatResponseStars rec) <> ", " <> "boardStatResponseViews: " <> show (boardStatResponseViews rec)

data BoardStatResponses = BoardStatResponses {
  boardStatResponses :: !([BoardStatResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardStatResponses where
  parseJSON (Object o) = do
    boardStatResponses <- o .: ("board_stat_responses" :: Text)
    pure $ BoardStatResponses {
      boardStatResponses = boardStatResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BoardStatResponses where
  toJSON BoardStatResponses{..} = object $
    [ "tag" .= ("BoardStatResponses" :: Text)
    , "board_stat_responses" .= boardStatResponses
    ]


instance Eq BoardStatResponses where
  (==) a b = boardStatResponses a == boardStatResponses b

instance Show BoardStatResponses where
    show rec = "boardStatResponses: " <> show (boardStatResponses rec)
-- footer