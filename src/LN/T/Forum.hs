{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Forum where


import LN.T.Visibility


import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Int            (Int64)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)
import           Data.Default

data ForumRequest = ForumRequest {
  forumRequestDisplayName :: Text,
  forumRequestDescription :: (Maybe Text),
  forumRequestThreadsPerBoard :: Int,
  forumRequestThreadPostsPerThread :: Int,
  forumRequestRecentThreadsLimit :: Int,
  forumRequestRecentPostsLimit :: Int,
  forumRequestMotwLimit :: Int,
  forumRequestIcon :: (Maybe Text),
  forumRequestTags :: [Text],
  forumRequestVisibility :: Visibility,
  forumRequestGuard :: Int
}


instance FromJSON ForumRequest where
  parseJSON (Object o) = do
    forumRequestDisplayName <- o .: ("display_name" :: Text)
    forumRequestDescription <- o .: ("description" :: Text)
    forumRequestThreadsPerBoard <- o .: ("threads_per_board" :: Text)
    forumRequestThreadPostsPerThread <- o .: ("thread_posts_per_thread" :: Text)
    forumRequestRecentThreadsLimit <- o .: ("recent_threads_limit" :: Text)
    forumRequestRecentPostsLimit <- o .: ("recent_posts_limit" :: Text)
    forumRequestMotwLimit <- o .: ("motw_limit" :: Text)
    forumRequestIcon <- o .: ("icon" :: Text)
    forumRequestTags <- o .: ("tags" :: Text)
    forumRequestVisibility <- o .: ("visibility" :: Text)
    forumRequestGuard <- o .: ("guard" :: Text)
    pure $ ForumRequest {
      forumRequestDisplayName = forumRequestDisplayName,
      forumRequestDescription = forumRequestDescription,
      forumRequestThreadsPerBoard = forumRequestThreadsPerBoard,
      forumRequestThreadPostsPerThread = forumRequestThreadPostsPerThread,
      forumRequestRecentThreadsLimit = forumRequestRecentThreadsLimit,
      forumRequestRecentPostsLimit = forumRequestRecentPostsLimit,
      forumRequestMotwLimit = forumRequestMotwLimit,
      forumRequestIcon = forumRequestIcon,
      forumRequestTags = forumRequestTags,
      forumRequestVisibility = forumRequestVisibility,
      forumRequestGuard = forumRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ForumRequest where
  toJSON ForumRequest{..} = object $
    [ "tag" .= ("ForumRequest" :: Text)
    , "display_name" .= forumRequestDisplayName
    , "description" .= forumRequestDescription
    , "threads_per_board" .= forumRequestThreadsPerBoard
    , "thread_posts_per_thread" .= forumRequestThreadPostsPerThread
    , "recent_threads_limit" .= forumRequestRecentThreadsLimit
    , "recent_posts_limit" .= forumRequestRecentPostsLimit
    , "motw_limit" .= forumRequestMotwLimit
    , "icon" .= forumRequestIcon
    , "tags" .= forumRequestTags
    , "visibility" .= forumRequestVisibility
    , "guard" .= forumRequestGuard
    ]


instance Eq ForumRequest where
  (==) a b = forumRequestDisplayName a == forumRequestDisplayName b && forumRequestDescription a == forumRequestDescription b && forumRequestThreadsPerBoard a == forumRequestThreadsPerBoard b && forumRequestThreadPostsPerThread a == forumRequestThreadPostsPerThread b && forumRequestRecentThreadsLimit a == forumRequestRecentThreadsLimit b && forumRequestRecentPostsLimit a == forumRequestRecentPostsLimit b && forumRequestMotwLimit a == forumRequestMotwLimit b && forumRequestIcon a == forumRequestIcon b && forumRequestTags a == forumRequestTags b && forumRequestVisibility a == forumRequestVisibility b && forumRequestGuard a == forumRequestGuard b

instance Show ForumRequest where
    show rec = "forumRequestDisplayName: " <> show (forumRequestDisplayName rec) <> ", " <> "forumRequestDescription: " <> show (forumRequestDescription rec) <> ", " <> "forumRequestThreadsPerBoard: " <> show (forumRequestThreadsPerBoard rec) <> ", " <> "forumRequestThreadPostsPerThread: " <> show (forumRequestThreadPostsPerThread rec) <> ", " <> "forumRequestRecentThreadsLimit: " <> show (forumRequestRecentThreadsLimit rec) <> ", " <> "forumRequestRecentPostsLimit: " <> show (forumRequestRecentPostsLimit rec) <> ", " <> "forumRequestMotwLimit: " <> show (forumRequestMotwLimit rec) <> ", " <> "forumRequestIcon: " <> show (forumRequestIcon rec) <> ", " <> "forumRequestTags: " <> show (forumRequestTags rec) <> ", " <> "forumRequestVisibility: " <> show (forumRequestVisibility rec) <> ", " <> "forumRequestGuard: " <> show (forumRequestGuard rec)

data ForumResponse = ForumResponse {
  forumResponseId :: Int64,
  forumResponseUserId :: Int64,
  forumResponseOrgId :: Int64,
  forumResponseName :: Text,
  forumResponseDisplayName :: Text,
  forumResponseDescription :: (Maybe Text),
  forumResponseThreadsPerBoard :: Int,
  forumResponseThreadPostsPerThread :: Int,
  forumResponseRecentThreadsLimit :: Int,
  forumResponseRecentPostsLimit :: Int,
  forumResponseMotwLimit :: Int,
  forumResponseIcon :: (Maybe Text),
  forumResponseTags :: [Text],
  forumResponseVisibility :: Visibility,
  forumResponseActive :: Bool,
  forumResponseGuard :: Int,
  forumResponseCreatedAt :: (Maybe UTCTime),
  forumResponseModifiedBy :: (Maybe Int64),
  forumResponseModifiedAt :: (Maybe UTCTime),
  forumResponseActivityAt :: (Maybe UTCTime)
}


instance FromJSON ForumResponse where
  parseJSON (Object o) = do
    forumResponseId <- o .: ("id" :: Text)
    forumResponseUserId <- o .: ("user_id" :: Text)
    forumResponseOrgId <- o .: ("org_id" :: Text)
    forumResponseName <- o .: ("name" :: Text)
    forumResponseDisplayName <- o .: ("display_name" :: Text)
    forumResponseDescription <- o .: ("description" :: Text)
    forumResponseThreadsPerBoard <- o .: ("threads_per_board" :: Text)
    forumResponseThreadPostsPerThread <- o .: ("thread_posts_per_thread" :: Text)
    forumResponseRecentThreadsLimit <- o .: ("recent_threads_limit" :: Text)
    forumResponseRecentPostsLimit <- o .: ("recent_posts_limit" :: Text)
    forumResponseMotwLimit <- o .: ("motw_limit" :: Text)
    forumResponseIcon <- o .: ("icon" :: Text)
    forumResponseTags <- o .: ("tags" :: Text)
    forumResponseVisibility <- o .: ("visibility" :: Text)
    forumResponseActive <- o .: ("active" :: Text)
    forumResponseGuard <- o .: ("guard" :: Text)
    forumResponseCreatedAt <- o .: ("created_at" :: Text)
    forumResponseModifiedBy <- o .: ("modified_by" :: Text)
    forumResponseModifiedAt <- o .: ("modified_at" :: Text)
    forumResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ ForumResponse {
      forumResponseId = forumResponseId,
      forumResponseUserId = forumResponseUserId,
      forumResponseOrgId = forumResponseOrgId,
      forumResponseName = forumResponseName,
      forumResponseDisplayName = forumResponseDisplayName,
      forumResponseDescription = forumResponseDescription,
      forumResponseThreadsPerBoard = forumResponseThreadsPerBoard,
      forumResponseThreadPostsPerThread = forumResponseThreadPostsPerThread,
      forumResponseRecentThreadsLimit = forumResponseRecentThreadsLimit,
      forumResponseRecentPostsLimit = forumResponseRecentPostsLimit,
      forumResponseMotwLimit = forumResponseMotwLimit,
      forumResponseIcon = forumResponseIcon,
      forumResponseTags = forumResponseTags,
      forumResponseVisibility = forumResponseVisibility,
      forumResponseActive = forumResponseActive,
      forumResponseGuard = forumResponseGuard,
      forumResponseCreatedAt = forumResponseCreatedAt,
      forumResponseModifiedBy = forumResponseModifiedBy,
      forumResponseModifiedAt = forumResponseModifiedAt,
      forumResponseActivityAt = forumResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ForumResponse where
  toJSON ForumResponse{..} = object $
    [ "tag" .= ("ForumResponse" :: Text)
    , "id" .= forumResponseId
    , "user_id" .= forumResponseUserId
    , "org_id" .= forumResponseOrgId
    , "name" .= forumResponseName
    , "display_name" .= forumResponseDisplayName
    , "description" .= forumResponseDescription
    , "threads_per_board" .= forumResponseThreadsPerBoard
    , "thread_posts_per_thread" .= forumResponseThreadPostsPerThread
    , "recent_threads_limit" .= forumResponseRecentThreadsLimit
    , "recent_posts_limit" .= forumResponseRecentPostsLimit
    , "motw_limit" .= forumResponseMotwLimit
    , "icon" .= forumResponseIcon
    , "tags" .= forumResponseTags
    , "visibility" .= forumResponseVisibility
    , "active" .= forumResponseActive
    , "guard" .= forumResponseGuard
    , "created_at" .= forumResponseCreatedAt
    , "modified_by" .= forumResponseModifiedBy
    , "modified_at" .= forumResponseModifiedAt
    , "activity_at" .= forumResponseActivityAt
    ]


instance Eq ForumResponse where
  (==) a b = forumResponseId a == forumResponseId b && forumResponseUserId a == forumResponseUserId b && forumResponseOrgId a == forumResponseOrgId b && forumResponseName a == forumResponseName b && forumResponseDisplayName a == forumResponseDisplayName b && forumResponseDescription a == forumResponseDescription b && forumResponseThreadsPerBoard a == forumResponseThreadsPerBoard b && forumResponseThreadPostsPerThread a == forumResponseThreadPostsPerThread b && forumResponseRecentThreadsLimit a == forumResponseRecentThreadsLimit b && forumResponseRecentPostsLimit a == forumResponseRecentPostsLimit b && forumResponseMotwLimit a == forumResponseMotwLimit b && forumResponseIcon a == forumResponseIcon b && forumResponseTags a == forumResponseTags b && forumResponseVisibility a == forumResponseVisibility b && forumResponseActive a == forumResponseActive b && forumResponseGuard a == forumResponseGuard b && forumResponseCreatedAt a == forumResponseCreatedAt b && forumResponseModifiedBy a == forumResponseModifiedBy b && forumResponseModifiedAt a == forumResponseModifiedAt b && forumResponseActivityAt a == forumResponseActivityAt b

instance Show ForumResponse where
    show rec = "forumResponseId: " <> show (forumResponseId rec) <> ", " <> "forumResponseUserId: " <> show (forumResponseUserId rec) <> ", " <> "forumResponseOrgId: " <> show (forumResponseOrgId rec) <> ", " <> "forumResponseName: " <> show (forumResponseName rec) <> ", " <> "forumResponseDisplayName: " <> show (forumResponseDisplayName rec) <> ", " <> "forumResponseDescription: " <> show (forumResponseDescription rec) <> ", " <> "forumResponseThreadsPerBoard: " <> show (forumResponseThreadsPerBoard rec) <> ", " <> "forumResponseThreadPostsPerThread: " <> show (forumResponseThreadPostsPerThread rec) <> ", " <> "forumResponseRecentThreadsLimit: " <> show (forumResponseRecentThreadsLimit rec) <> ", " <> "forumResponseRecentPostsLimit: " <> show (forumResponseRecentPostsLimit rec) <> ", " <> "forumResponseMotwLimit: " <> show (forumResponseMotwLimit rec) <> ", " <> "forumResponseIcon: " <> show (forumResponseIcon rec) <> ", " <> "forumResponseTags: " <> show (forumResponseTags rec) <> ", " <> "forumResponseVisibility: " <> show (forumResponseVisibility rec) <> ", " <> "forumResponseActive: " <> show (forumResponseActive rec) <> ", " <> "forumResponseGuard: " <> show (forumResponseGuard rec) <> ", " <> "forumResponseCreatedAt: " <> show (forumResponseCreatedAt rec) <> ", " <> "forumResponseModifiedBy: " <> show (forumResponseModifiedBy rec) <> ", " <> "forumResponseModifiedAt: " <> show (forumResponseModifiedAt rec) <> ", " <> "forumResponseActivityAt: " <> show (forumResponseActivityAt rec)

data ForumResponses = ForumResponses {
  forumResponses :: [ForumResponse]
}


instance FromJSON ForumResponses where
  parseJSON (Object o) = do
    forumResponses <- o .: ("forum_responses" :: Text)
    pure $ ForumResponses {
      forumResponses = forumResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ForumResponses where
  toJSON ForumResponses{..} = object $
    [ "tag" .= ("ForumResponses" :: Text)
    , "forum_responses" .= forumResponses
    ]


instance Eq ForumResponses where
  (==) a b = forumResponses a == forumResponses b

instance Show ForumResponses where
    show rec = "forumResponses: " <> show (forumResponses rec)

data ForumStatResponse = ForumStatResponse {
  forumStatResponseForumId :: Int64,
  forumStatResponseBoards :: Int64,
  forumStatResponseThreads :: Int64,
  forumStatResponseThreadPosts :: Int64,
  forumStatResponseViews :: Int64
}


instance FromJSON ForumStatResponse where
  parseJSON (Object o) = do
    forumStatResponseForumId <- o .: ("forum_id" :: Text)
    forumStatResponseBoards <- o .: ("boards" :: Text)
    forumStatResponseThreads <- o .: ("threads" :: Text)
    forumStatResponseThreadPosts <- o .: ("thread_posts" :: Text)
    forumStatResponseViews <- o .: ("views" :: Text)
    pure $ ForumStatResponse {
      forumStatResponseForumId = forumStatResponseForumId,
      forumStatResponseBoards = forumStatResponseBoards,
      forumStatResponseThreads = forumStatResponseThreads,
      forumStatResponseThreadPosts = forumStatResponseThreadPosts,
      forumStatResponseViews = forumStatResponseViews
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ForumStatResponse where
  toJSON ForumStatResponse{..} = object $
    [ "tag" .= ("ForumStatResponse" :: Text)
    , "forum_id" .= forumStatResponseForumId
    , "boards" .= forumStatResponseBoards
    , "threads" .= forumStatResponseThreads
    , "thread_posts" .= forumStatResponseThreadPosts
    , "views" .= forumStatResponseViews
    ]


instance Eq ForumStatResponse where
  (==) a b = forumStatResponseForumId a == forumStatResponseForumId b && forumStatResponseBoards a == forumStatResponseBoards b && forumStatResponseThreads a == forumStatResponseThreads b && forumStatResponseThreadPosts a == forumStatResponseThreadPosts b && forumStatResponseViews a == forumStatResponseViews b

instance Show ForumStatResponse where
    show rec = "forumStatResponseForumId: " <> show (forumStatResponseForumId rec) <> ", " <> "forumStatResponseBoards: " <> show (forumStatResponseBoards rec) <> ", " <> "forumStatResponseThreads: " <> show (forumStatResponseThreads rec) <> ", " <> "forumStatResponseThreadPosts: " <> show (forumStatResponseThreadPosts rec) <> ", " <> "forumStatResponseViews: " <> show (forumStatResponseViews rec)

data ForumStatResponses = ForumStatResponses {
  forumStatResponses :: [ForumStatResponse]
}


instance FromJSON ForumStatResponses where
  parseJSON (Object o) = do
    forumStatResponses <- o .: ("forum_stat_responses" :: Text)
    pure $ ForumStatResponses {
      forumStatResponses = forumStatResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ForumStatResponses where
  toJSON ForumStatResponses{..} = object $
    [ "tag" .= ("ForumStatResponses" :: Text)
    , "forum_stat_responses" .= forumStatResponses
    ]


instance Eq ForumStatResponses where
  (==) a b = forumStatResponses a == forumStatResponses b

instance Show ForumStatResponses where
    show rec = "forumStatResponses: " <> show (forumStatResponses rec)
-- footer