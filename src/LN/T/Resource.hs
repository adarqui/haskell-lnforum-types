{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Resource where




import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)

instance FromJSON ResourceType where
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

      _ -> fail "Could not parse ResourceType"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ResourceType where
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


instance Eq ResourceType where
  (==) (ISBN13 x0a) (ISBN13 x0b) = x0a == x0b
  (==) (ISBN10 x0a) (ISBN10 x0b) = x0a == x0b
  (==) (ISBN x0a) (ISBN x0b) = x0a == x0b
  (==) (URL x0a) (URL x0b) = x0a == x0b
  (==) SourceNone SourceNone = True
  (==) _ _ = False

instance Show ResourceType where
  show (ISBN13 x0) = "isbn13: " <> show x0
  show (ISBN10 x0) = "isbn10: " <> show x0
  show (ISBN x0) = "isbn: " <> show x0
  show (URL x0) = "url: " <> show x0
  show SourceNone = "source_none"


instance FromJSON TyResourceType where
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

      _ -> fail "Could not parse TyResourceType"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON TyResourceType where
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


instance Eq TyResourceType where
  (==) TyISBN13 TyISBN13 = True
  (==) TyISBN10 TyISBN10 = True
  (==) TyISBN TyISBN = True
  (==) TyURL TyURL = True
  (==) TySourceNone TySourceNone = True
  (==) _ _ = False

instance Show TyResourceType where
  show TyISBN13 = "ty_isbn13"
  show TyISBN10 = "ty_isbn10"
  show TyISBN = "ty_isbn"
  show TyURL = "ty_url"
  show TySourceNone = "ty_source_none"


instance FromJSON ResourceRequest where
  parseJSON (Object o) = do
    resourceRequestDisplayName <- o .: ("display_name" :: Text)
    resourceRequestDescription <- o .: ("description" :: Text)
    resourceRequestSource <- o .: ("source" :: Text)
    resourceRequestAuthor <- o .: ("author" :: Text)
    resourceRequestPrerequisites <- o .: ("prerequisites" :: Text)
    resourceRequestCategories <- o .: ("categories" :: Text)
    resourceRequestVisibility <- o .: ("visibility" :: Text)
    resourceRequestCounter <- o .: ("counter" :: Text)
    resourceRequestVersion <- o .: ("version" :: Text)
    resourceRequestUrls <- o .: ("urls" :: Text)
    resourceRequestIcon <- o .: ("icon" :: Text)
    resourceRequestTags <- o .: ("tags" :: Text)
    resourceRequestGuard <- o .: ("guard" :: Text)
    pure $ ResourceRequest {
      resourceRequestDisplayName = resourceRequestDisplayName,
      resourceRequestDescription = resourceRequestDescription,
      resourceRequestSource = resourceRequestSource,
      resourceRequestAuthor = resourceRequestAuthor,
      resourceRequestPrerequisites = resourceRequestPrerequisites,
      resourceRequestCategories = resourceRequestCategories,
      resourceRequestVisibility = resourceRequestVisibility,
      resourceRequestCounter = resourceRequestCounter,
      resourceRequestVersion = resourceRequestVersion,
      resourceRequestUrls = resourceRequestUrls,
      resourceRequestIcon = resourceRequestIcon,
      resourceRequestTags = resourceRequestTags,
      resourceRequestGuard = resourceRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ResourceRequest where
  toJSON ResourceRequest{..} = object $
    [ "tag" .= ("ResourceRequest" :: Text)
    , "display_name" .= resourceRequestDisplayName
    , "description" .= resourceRequestDescription
    , "source" .= resourceRequestSource
    , "author" .= resourceRequestAuthor
    , "prerequisites" .= resourceRequestPrerequisites
    , "categories" .= resourceRequestCategories
    , "visibility" .= resourceRequestVisibility
    , "counter" .= resourceRequestCounter
    , "version" .= resourceRequestVersion
    , "urls" .= resourceRequestUrls
    , "icon" .= resourceRequestIcon
    , "tags" .= resourceRequestTags
    , "guard" .= resourceRequestGuard
    ]


instance Eq ResourceRequest where
  (==) a b = resourceRequestDisplayName a == resourceRequestDisplayName b && resourceRequestDescription a == resourceRequestDescription b && resourceRequestSource a == resourceRequestSource b && resourceRequestAuthor a == resourceRequestAuthor b && resourceRequestPrerequisites a == resourceRequestPrerequisites b && resourceRequestCategories a == resourceRequestCategories b && resourceRequestVisibility a == resourceRequestVisibility b && resourceRequestCounter a == resourceRequestCounter b && resourceRequestVersion a == resourceRequestVersion b && resourceRequestUrls a == resourceRequestUrls b && resourceRequestIcon a == resourceRequestIcon b && resourceRequestTags a == resourceRequestTags b && resourceRequestGuard a == resourceRequestGuard b

instance Show ResourceRequest where
    show rec = "resourceRequestDisplayName: " <> show (resourceRequestDisplayName rec) <> ", " <> "resourceRequestDescription: " <> show (resourceRequestDescription rec) <> ", " <> "resourceRequestSource: " <> show (resourceRequestSource rec) <> ", " <> "resourceRequestAuthor: " <> show (resourceRequestAuthor rec) <> ", " <> "resourceRequestPrerequisites: " <> show (resourceRequestPrerequisites rec) <> ", " <> "resourceRequestCategories: " <> show (resourceRequestCategories rec) <> ", " <> "resourceRequestVisibility: " <> show (resourceRequestVisibility rec) <> ", " <> "resourceRequestCounter: " <> show (resourceRequestCounter rec) <> ", " <> "resourceRequestVersion: " <> show (resourceRequestVersion rec) <> ", " <> "resourceRequestUrls: " <> show (resourceRequestUrls rec) <> ", " <> "resourceRequestIcon: " <> show (resourceRequestIcon rec) <> ", " <> "resourceRequestTags: " <> show (resourceRequestTags rec) <> ", " <> "resourceRequestGuard: " <> show (resourceRequestGuard rec)

instance FromJSON ResourceResponse where
  parseJSON (Object o) = do
    resourceResponseId <- o .: ("id" :: Text)
    resourceResponseUserId <- o .: ("user_id" :: Text)
    resourceResponseName <- o .: ("name" :: Text)
    resourceResponseDisplayName <- o .: ("display_name" :: Text)
    resourceResponseDescription <- o .: ("description" :: Text)
    resourceResponseSource <- o .: ("source" :: Text)
    resourceResponseAuthor <- o .: ("author" :: Text)
    resourceResponsePrerequisites <- o .: ("prerequisites" :: Text)
    resourceResponseCategories <- o .: ("categories" :: Text)
    resourceResponseVisibility <- o .: ("visibility" :: Text)
    resourceResponseCounter <- o .: ("counter" :: Text)
    resourceResponseVersion <- o .: ("version" :: Text)
    resourceResponseUrls <- o .: ("urls" :: Text)
    resourceResponseIcon <- o .: ("icon" :: Text)
    resourceResponseTags <- o .: ("tags" :: Text)
    resourceResponseActive <- o .: ("active" :: Text)
    resourceResponseGuard <- o .: ("guard" :: Text)
    resourceResponseCreatedAt <- o .: ("created_at" :: Text)
    resourceResponseModifiedAt <- o .: ("modified_at" :: Text)
    resourceResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ ResourceResponse {
      resourceResponseId = resourceResponseId,
      resourceResponseUserId = resourceResponseUserId,
      resourceResponseName = resourceResponseName,
      resourceResponseDisplayName = resourceResponseDisplayName,
      resourceResponseDescription = resourceResponseDescription,
      resourceResponseSource = resourceResponseSource,
      resourceResponseAuthor = resourceResponseAuthor,
      resourceResponsePrerequisites = resourceResponsePrerequisites,
      resourceResponseCategories = resourceResponseCategories,
      resourceResponseVisibility = resourceResponseVisibility,
      resourceResponseCounter = resourceResponseCounter,
      resourceResponseVersion = resourceResponseVersion,
      resourceResponseUrls = resourceResponseUrls,
      resourceResponseIcon = resourceResponseIcon,
      resourceResponseTags = resourceResponseTags,
      resourceResponseActive = resourceResponseActive,
      resourceResponseGuard = resourceResponseGuard,
      resourceResponseCreatedAt = resourceResponseCreatedAt,
      resourceResponseModifiedAt = resourceResponseModifiedAt,
      resourceResponseActivityAt = resourceResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ResourceResponse where
  toJSON ResourceResponse{..} = object $
    [ "tag" .= ("ResourceResponse" :: Text)
    , "id" .= resourceResponseId
    , "user_id" .= resourceResponseUserId
    , "name" .= resourceResponseName
    , "display_name" .= resourceResponseDisplayName
    , "description" .= resourceResponseDescription
    , "source" .= resourceResponseSource
    , "author" .= resourceResponseAuthor
    , "prerequisites" .= resourceResponsePrerequisites
    , "categories" .= resourceResponseCategories
    , "visibility" .= resourceResponseVisibility
    , "counter" .= resourceResponseCounter
    , "version" .= resourceResponseVersion
    , "urls" .= resourceResponseUrls
    , "icon" .= resourceResponseIcon
    , "tags" .= resourceResponseTags
    , "active" .= resourceResponseActive
    , "guard" .= resourceResponseGuard
    , "created_at" .= resourceResponseCreatedAt
    , "modified_at" .= resourceResponseModifiedAt
    , "activity_at" .= resourceResponseActivityAt
    ]


instance Eq ResourceResponse where
  (==) a b = resourceResponseId a == resourceResponseId b && resourceResponseUserId a == resourceResponseUserId b && resourceResponseName a == resourceResponseName b && resourceResponseDisplayName a == resourceResponseDisplayName b && resourceResponseDescription a == resourceResponseDescription b && resourceResponseSource a == resourceResponseSource b && resourceResponseAuthor a == resourceResponseAuthor b && resourceResponsePrerequisites a == resourceResponsePrerequisites b && resourceResponseCategories a == resourceResponseCategories b && resourceResponseVisibility a == resourceResponseVisibility b && resourceResponseCounter a == resourceResponseCounter b && resourceResponseVersion a == resourceResponseVersion b && resourceResponseUrls a == resourceResponseUrls b && resourceResponseIcon a == resourceResponseIcon b && resourceResponseTags a == resourceResponseTags b && resourceResponseActive a == resourceResponseActive b && resourceResponseGuard a == resourceResponseGuard b && resourceResponseCreatedAt a == resourceResponseCreatedAt b && resourceResponseModifiedAt a == resourceResponseModifiedAt b && resourceResponseActivityAt a == resourceResponseActivityAt b

instance Show ResourceResponse where
    show rec = "resourceResponseId: " <> show (resourceResponseId rec) <> ", " <> "resourceResponseUserId: " <> show (resourceResponseUserId rec) <> ", " <> "resourceResponseName: " <> show (resourceResponseName rec) <> ", " <> "resourceResponseDisplayName: " <> show (resourceResponseDisplayName rec) <> ", " <> "resourceResponseDescription: " <> show (resourceResponseDescription rec) <> ", " <> "resourceResponseSource: " <> show (resourceResponseSource rec) <> ", " <> "resourceResponseAuthor: " <> show (resourceResponseAuthor rec) <> ", " <> "resourceResponsePrerequisites: " <> show (resourceResponsePrerequisites rec) <> ", " <> "resourceResponseCategories: " <> show (resourceResponseCategories rec) <> ", " <> "resourceResponseVisibility: " <> show (resourceResponseVisibility rec) <> ", " <> "resourceResponseCounter: " <> show (resourceResponseCounter rec) <> ", " <> "resourceResponseVersion: " <> show (resourceResponseVersion rec) <> ", " <> "resourceResponseUrls: " <> show (resourceResponseUrls rec) <> ", " <> "resourceResponseIcon: " <> show (resourceResponseIcon rec) <> ", " <> "resourceResponseTags: " <> show (resourceResponseTags rec) <> ", " <> "resourceResponseActive: " <> show (resourceResponseActive rec) <> ", " <> "resourceResponseGuard: " <> show (resourceResponseGuard rec) <> ", " <> "resourceResponseCreatedAt: " <> show (resourceResponseCreatedAt rec) <> ", " <> "resourceResponseModifiedAt: " <> show (resourceResponseModifiedAt rec) <> ", " <> "resourceResponseActivityAt: " <> show (resourceResponseActivityAt rec)

instance FromJSON ResourceResponses where
  parseJSON (Object o) = do
    resourceResponses <- o .: ("resource_responses" :: Text)
    pure $ ResourceResponses {
      resourceResponses = resourceResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ResourceResponses where
  toJSON ResourceResponses{..} = object $
    [ "tag" .= ("ResourceResponses" :: Text)
    , "resource_responses" .= resourceResponses
    ]


instance Eq ResourceResponses where
  (==) a b = resourceResponses a == resourceResponses b

instance Show ResourceResponses where
    show rec = "resourceResponses: " <> show (resourceResponses rec)

instance FromJSON ResourceStatResponse where
  parseJSON (Object o) = do
    resourceStatResponseResourceId <- o .: ("resource_id" :: Text)
    resourceStatResponseLeurons <- o .: ("leurons" :: Text)
    resourceStatResponseLikes <- o .: ("likes" :: Text)
    resourceStatResponseNeutral <- o .: ("neutral" :: Text)
    resourceStatResponseDislikes <- o .: ("dislikes" :: Text)
    resourceStatResponseStars <- o .: ("stars" :: Text)
    resourceStatResponseViews <- o .: ("views" :: Text)
    pure $ ResourceStatResponse {
      resourceStatResponseResourceId = resourceStatResponseResourceId,
      resourceStatResponseLeurons = resourceStatResponseLeurons,
      resourceStatResponseLikes = resourceStatResponseLikes,
      resourceStatResponseNeutral = resourceStatResponseNeutral,
      resourceStatResponseDislikes = resourceStatResponseDislikes,
      resourceStatResponseStars = resourceStatResponseStars,
      resourceStatResponseViews = resourceStatResponseViews
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ResourceStatResponse where
  toJSON ResourceStatResponse{..} = object $
    [ "tag" .= ("ResourceStatResponse" :: Text)
    , "resource_id" .= resourceStatResponseResourceId
    , "leurons" .= resourceStatResponseLeurons
    , "likes" .= resourceStatResponseLikes
    , "neutral" .= resourceStatResponseNeutral
    , "dislikes" .= resourceStatResponseDislikes
    , "stars" .= resourceStatResponseStars
    , "views" .= resourceStatResponseViews
    ]


instance Eq ResourceStatResponse where
  (==) a b = resourceStatResponseResourceId a == resourceStatResponseResourceId b && resourceStatResponseLeurons a == resourceStatResponseLeurons b && resourceStatResponseLikes a == resourceStatResponseLikes b && resourceStatResponseNeutral a == resourceStatResponseNeutral b && resourceStatResponseDislikes a == resourceStatResponseDislikes b && resourceStatResponseStars a == resourceStatResponseStars b && resourceStatResponseViews a == resourceStatResponseViews b

instance Show ResourceStatResponse where
    show rec = "resourceStatResponseResourceId: " <> show (resourceStatResponseResourceId rec) <> ", " <> "resourceStatResponseLeurons: " <> show (resourceStatResponseLeurons rec) <> ", " <> "resourceStatResponseLikes: " <> show (resourceStatResponseLikes rec) <> ", " <> "resourceStatResponseNeutral: " <> show (resourceStatResponseNeutral rec) <> ", " <> "resourceStatResponseDislikes: " <> show (resourceStatResponseDislikes rec) <> ", " <> "resourceStatResponseStars: " <> show (resourceStatResponseStars rec) <> ", " <> "resourceStatResponseViews: " <> show (resourceStatResponseViews rec)

instance FromJSON ResourceStatResponses where
  parseJSON (Object o) = do
    resourceStatResponses <- o .: ("resource_stat_responses" :: Text)
    pure $ ResourceStatResponses {
      resourceStatResponses = resourceStatResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ResourceStatResponses where
  toJSON ResourceStatResponses{..} = object $
    [ "tag" .= ("ResourceStatResponses" :: Text)
    , "resource_stat_responses" .= resourceStatResponses
    ]


instance Eq ResourceStatResponses where
  (==) a b = resourceStatResponses a == resourceStatResponses b

instance Show ResourceStatResponses where
    show rec = "resourceStatResponses: " <> show (resourceStatResponses rec)
-- footer