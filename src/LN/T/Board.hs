{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Board where





import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Int            (Int64)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)

data BoardRequest = BoardRequest {
  boardRequestDisplayName :: Text,
  boardRequestDescription :: (Maybe Text),
  boardRequestIsAnonymous :: Bool,
  boardRequestCanCreateSubBoards :: Bool,
  boardRequestCanCreateThreads :: Bool,
  boardRequestSuggestedTags :: [Text],
  boardRequestIcon :: (Maybe Text),
  boardRequestTags :: [Text],
  boardRequestGuard :: Int
}


instance FromJSON BoardRequest where
  parseJSON (Object o) = do
    boardRequestDisplayName <- o .: ("display_name" :: Text)
    boardRequestDescription <- o .: ("description" :: Text)
    boardRequestIsAnonymous <- o .: ("is_anonymous" :: Text)
    boardRequestCanCreateSubBoards <- o .: ("can_create_sub_boards" :: Text)
    boardRequestCanCreateThreads <- o .: ("can_create_threads" :: Text)
    boardRequestSuggestedTags <- o .: ("suggested_tags" :: Text)
    boardRequestIcon <- o .: ("icon" :: Text)
    boardRequestTags <- o .: ("tags" :: Text)
    boardRequestGuard <- o .: ("guard" :: Text)
    pure $ BoardRequest {
      boardRequestDisplayName = boardRequestDisplayName,
      boardRequestDescription = boardRequestDescription,
      boardRequestIsAnonymous = boardRequestIsAnonymous,
      boardRequestCanCreateSubBoards = boardRequestCanCreateSubBoards,
      boardRequestCanCreateThreads = boardRequestCanCreateThreads,
      boardRequestSuggestedTags = boardRequestSuggestedTags,
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
    , "is_anonymous" .= boardRequestIsAnonymous
    , "can_create_sub_boards" .= boardRequestCanCreateSubBoards
    , "can_create_threads" .= boardRequestCanCreateThreads
    , "suggested_tags" .= boardRequestSuggestedTags
    , "icon" .= boardRequestIcon
    , "tags" .= boardRequestTags
    , "guard" .= boardRequestGuard
    ]


instance Eq BoardRequest where
  (==) a b = boardRequestDisplayName a == boardRequestDisplayName b && boardRequestDescription a == boardRequestDescription b && boardRequestIsAnonymous a == boardRequestIsAnonymous b && boardRequestCanCreateSubBoards a == boardRequestCanCreateSubBoards b && boardRequestCanCreateThreads a == boardRequestCanCreateThreads b && boardRequestSuggestedTags a == boardRequestSuggestedTags b && boardRequestIcon a == boardRequestIcon b && boardRequestTags a == boardRequestTags b && boardRequestGuard a == boardRequestGuard b

instance Show BoardRequest where
    show rec = "boardRequestDisplayName: " <> show (boardRequestDisplayName rec) <> ", " <> "boardRequestDescription: " <> show (boardRequestDescription rec) <> ", " <> "boardRequestIsAnonymous: " <> show (boardRequestIsAnonymous rec) <> ", " <> "boardRequestCanCreateSubBoards: " <> show (boardRequestCanCreateSubBoards rec) <> ", " <> "boardRequestCanCreateThreads: " <> show (boardRequestCanCreateThreads rec) <> ", " <> "boardRequestSuggestedTags: " <> show (boardRequestSuggestedTags rec) <> ", " <> "boardRequestIcon: " <> show (boardRequestIcon rec) <> ", " <> "boardRequestTags: " <> show (boardRequestTags rec) <> ", " <> "boardRequestGuard: " <> show (boardRequestGuard rec)

data BoardResponse = BoardResponse {
  boardResponseId :: Int64,
  boardResponseUserId :: Int64,
  boardResponseOrgId :: Int64,
  boardResponseForumId :: Int64,
  boardResponseParentId :: (Maybe Int64),
  boardResponseName :: Text,
  boardResponseDisplayName :: Text,
  boardResponseDescription :: (Maybe Text),
  boardResponseIsAnonymous :: Bool,
  boardResponseCanCreateSubBoards :: Bool,
  boardResponseCanCreateThreads :: Bool,
  boardResponseSuggestedTags :: [Text],
  boardResponseIcon :: (Maybe Text),
  boardResponseTags :: [Text],
  boardResponseActive :: Bool,
  boardResponseGuard :: Int,
  boardResponseCreatedAt :: (Maybe UTCTime),
  boardResponseModifiedBy :: (Maybe Int64),
  boardResponseModifiedAt :: (Maybe UTCTime),
  boardResponseActivityAt :: (Maybe UTCTime)
}


instance FromJSON BoardResponse where
  parseJSON (Object o) = do
    boardResponseId <- o .: ("id" :: Text)
    boardResponseUserId <- o .: ("user_id" :: Text)
    boardResponseOrgId <- o .: ("org_id" :: Text)
    boardResponseForumId <- o .: ("forum_id" :: Text)
    boardResponseParentId <- o .: ("parent_id" :: Text)
    boardResponseName <- o .: ("name" :: Text)
    boardResponseDisplayName <- o .: ("display_name" :: Text)
    boardResponseDescription <- o .: ("description" :: Text)
    boardResponseIsAnonymous <- o .: ("is_anonymous" :: Text)
    boardResponseCanCreateSubBoards <- o .: ("can_create_sub_boards" :: Text)
    boardResponseCanCreateThreads <- o .: ("can_create_threads" :: Text)
    boardResponseSuggestedTags <- o .: ("suggested_tags" :: Text)
    boardResponseIcon <- o .: ("icon" :: Text)
    boardResponseTags <- o .: ("tags" :: Text)
    boardResponseActive <- o .: ("active" :: Text)
    boardResponseGuard <- o .: ("guard" :: Text)
    boardResponseCreatedAt <- o .: ("created_at" :: Text)
    boardResponseModifiedBy <- o .: ("modified_by" :: Text)
    boardResponseModifiedAt <- o .: ("modified_at" :: Text)
    boardResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ BoardResponse {
      boardResponseId = boardResponseId,
      boardResponseUserId = boardResponseUserId,
      boardResponseOrgId = boardResponseOrgId,
      boardResponseForumId = boardResponseForumId,
      boardResponseParentId = boardResponseParentId,
      boardResponseName = boardResponseName,
      boardResponseDisplayName = boardResponseDisplayName,
      boardResponseDescription = boardResponseDescription,
      boardResponseIsAnonymous = boardResponseIsAnonymous,
      boardResponseCanCreateSubBoards = boardResponseCanCreateSubBoards,
      boardResponseCanCreateThreads = boardResponseCanCreateThreads,
      boardResponseSuggestedTags = boardResponseSuggestedTags,
      boardResponseIcon = boardResponseIcon,
      boardResponseTags = boardResponseTags,
      boardResponseActive = boardResponseActive,
      boardResponseGuard = boardResponseGuard,
      boardResponseCreatedAt = boardResponseCreatedAt,
      boardResponseModifiedBy = boardResponseModifiedBy,
      boardResponseModifiedAt = boardResponseModifiedAt,
      boardResponseActivityAt = boardResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BoardResponse where
  toJSON BoardResponse{..} = object $
    [ "tag" .= ("BoardResponse" :: Text)
    , "id" .= boardResponseId
    , "user_id" .= boardResponseUserId
    , "org_id" .= boardResponseOrgId
    , "forum_id" .= boardResponseForumId
    , "parent_id" .= boardResponseParentId
    , "name" .= boardResponseName
    , "display_name" .= boardResponseDisplayName
    , "description" .= boardResponseDescription
    , "is_anonymous" .= boardResponseIsAnonymous
    , "can_create_sub_boards" .= boardResponseCanCreateSubBoards
    , "can_create_threads" .= boardResponseCanCreateThreads
    , "suggested_tags" .= boardResponseSuggestedTags
    , "icon" .= boardResponseIcon
    , "tags" .= boardResponseTags
    , "active" .= boardResponseActive
    , "guard" .= boardResponseGuard
    , "created_at" .= boardResponseCreatedAt
    , "modified_by" .= boardResponseModifiedBy
    , "modified_at" .= boardResponseModifiedAt
    , "activity_at" .= boardResponseActivityAt
    ]


instance Eq BoardResponse where
  (==) a b = boardResponseId a == boardResponseId b && boardResponseUserId a == boardResponseUserId b && boardResponseOrgId a == boardResponseOrgId b && boardResponseForumId a == boardResponseForumId b && boardResponseParentId a == boardResponseParentId b && boardResponseName a == boardResponseName b && boardResponseDisplayName a == boardResponseDisplayName b && boardResponseDescription a == boardResponseDescription b && boardResponseIsAnonymous a == boardResponseIsAnonymous b && boardResponseCanCreateSubBoards a == boardResponseCanCreateSubBoards b && boardResponseCanCreateThreads a == boardResponseCanCreateThreads b && boardResponseSuggestedTags a == boardResponseSuggestedTags b && boardResponseIcon a == boardResponseIcon b && boardResponseTags a == boardResponseTags b && boardResponseActive a == boardResponseActive b && boardResponseGuard a == boardResponseGuard b && boardResponseCreatedAt a == boardResponseCreatedAt b && boardResponseModifiedBy a == boardResponseModifiedBy b && boardResponseModifiedAt a == boardResponseModifiedAt b && boardResponseActivityAt a == boardResponseActivityAt b

instance Show BoardResponse where
    show rec = "boardResponseId: " <> show (boardResponseId rec) <> ", " <> "boardResponseUserId: " <> show (boardResponseUserId rec) <> ", " <> "boardResponseOrgId: " <> show (boardResponseOrgId rec) <> ", " <> "boardResponseForumId: " <> show (boardResponseForumId rec) <> ", " <> "boardResponseParentId: " <> show (boardResponseParentId rec) <> ", " <> "boardResponseName: " <> show (boardResponseName rec) <> ", " <> "boardResponseDisplayName: " <> show (boardResponseDisplayName rec) <> ", " <> "boardResponseDescription: " <> show (boardResponseDescription rec) <> ", " <> "boardResponseIsAnonymous: " <> show (boardResponseIsAnonymous rec) <> ", " <> "boardResponseCanCreateSubBoards: " <> show (boardResponseCanCreateSubBoards rec) <> ", " <> "boardResponseCanCreateThreads: " <> show (boardResponseCanCreateThreads rec) <> ", " <> "boardResponseSuggestedTags: " <> show (boardResponseSuggestedTags rec) <> ", " <> "boardResponseIcon: " <> show (boardResponseIcon rec) <> ", " <> "boardResponseTags: " <> show (boardResponseTags rec) <> ", " <> "boardResponseActive: " <> show (boardResponseActive rec) <> ", " <> "boardResponseGuard: " <> show (boardResponseGuard rec) <> ", " <> "boardResponseCreatedAt: " <> show (boardResponseCreatedAt rec) <> ", " <> "boardResponseModifiedBy: " <> show (boardResponseModifiedBy rec) <> ", " <> "boardResponseModifiedAt: " <> show (boardResponseModifiedAt rec) <> ", " <> "boardResponseActivityAt: " <> show (boardResponseActivityAt rec)

data BoardResponses = BoardResponses {
  boardResponses :: [BoardResponse]
}


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
  boardStatResponseBoardId :: Int64,
  boardStatResponseThreads :: Int64,
  boardStatResponseThreadPosts :: Int64,
  boardStatResponseViews :: Int64
}


instance FromJSON BoardStatResponse where
  parseJSON (Object o) = do
    boardStatResponseBoardId <- o .: ("board_id" :: Text)
    boardStatResponseThreads <- o .: ("threads" :: Text)
    boardStatResponseThreadPosts <- o .: ("thread_posts" :: Text)
    boardStatResponseViews <- o .: ("views" :: Text)
    pure $ BoardStatResponse {
      boardStatResponseBoardId = boardStatResponseBoardId,
      boardStatResponseThreads = boardStatResponseThreads,
      boardStatResponseThreadPosts = boardStatResponseThreadPosts,
      boardStatResponseViews = boardStatResponseViews
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BoardStatResponse where
  toJSON BoardStatResponse{..} = object $
    [ "tag" .= ("BoardStatResponse" :: Text)
    , "board_id" .= boardStatResponseBoardId
    , "threads" .= boardStatResponseThreads
    , "thread_posts" .= boardStatResponseThreadPosts
    , "views" .= boardStatResponseViews
    ]


instance Eq BoardStatResponse where
  (==) a b = boardStatResponseBoardId a == boardStatResponseBoardId b && boardStatResponseThreads a == boardStatResponseThreads b && boardStatResponseThreadPosts a == boardStatResponseThreadPosts b && boardStatResponseViews a == boardStatResponseViews b

instance Show BoardStatResponse where
    show rec = "boardStatResponseBoardId: " <> show (boardStatResponseBoardId rec) <> ", " <> "boardStatResponseThreads: " <> show (boardStatResponseThreads rec) <> ", " <> "boardStatResponseThreadPosts: " <> show (boardStatResponseThreadPosts rec) <> ", " <> "boardStatResponseViews: " <> show (boardStatResponseViews rec)

data BoardStatResponses = BoardStatResponses {
  boardStatResponses :: [BoardStatResponse]
}


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