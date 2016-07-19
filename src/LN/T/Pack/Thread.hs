{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.Thread where


import LN.T.Permission
import LN.T.Organization
import LN.T.User
import LN.T.Forum
import LN.T.Board
import LN.T.Thread
import LN.T.ThreadPost
import LN.T.Like
import LN.T.Star


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

data ThreadPackResponse = ThreadPackResponse {
  threadPackResponseThread :: !(ThreadResponse),
  threadPackResponseThreadId :: !(Int64),
  threadPackResponseUser :: !(UserSanitizedResponse),
  threadPackResponseUserId :: !(Int64),
  threadPackResponseStat :: !(ThreadStatResponse),
  threadPackResponseLike :: !((Maybe LikeResponse)),
  threadPackResponseStar :: !((Maybe StarResponse)),
  threadPackResponseLatestThreadPost :: !((Maybe ThreadPostResponse)),
  threadPackResponseLatestThreadPostUser :: !((Maybe UserSanitizedResponse)),
  threadPackResponseWithOrganization :: !((Maybe OrganizationResponse)),
  threadPackResponseWithForum :: !((Maybe ForumResponse)),
  threadPackResponseWithBoard :: !((Maybe BoardResponse)),
  threadPackResponsePermissions :: !(Permissions)
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadPackResponse where
  parseJSON (Object o) = do
    threadPackResponseThread <- o .: ("thread" :: Text)
    threadPackResponseThreadId <- o .: ("thread_id" :: Text)
    threadPackResponseUser <- o .: ("user" :: Text)
    threadPackResponseUserId <- o .: ("user_id" :: Text)
    threadPackResponseStat <- o .: ("stat" :: Text)
    threadPackResponseLike <- o .: ("like" :: Text)
    threadPackResponseStar <- o .: ("star" :: Text)
    threadPackResponseLatestThreadPost <- o .: ("latest_thread_post" :: Text)
    threadPackResponseLatestThreadPostUser <- o .: ("latest_thread_post_user" :: Text)
    threadPackResponseWithOrganization <- o .: ("with_organization" :: Text)
    threadPackResponseWithForum <- o .: ("with_forum" :: Text)
    threadPackResponseWithBoard <- o .: ("with_board" :: Text)
    threadPackResponsePermissions <- o .: ("permissions" :: Text)
    pure $ ThreadPackResponse {
      threadPackResponseThread = threadPackResponseThread,
      threadPackResponseThreadId = threadPackResponseThreadId,
      threadPackResponseUser = threadPackResponseUser,
      threadPackResponseUserId = threadPackResponseUserId,
      threadPackResponseStat = threadPackResponseStat,
      threadPackResponseLike = threadPackResponseLike,
      threadPackResponseStar = threadPackResponseStar,
      threadPackResponseLatestThreadPost = threadPackResponseLatestThreadPost,
      threadPackResponseLatestThreadPostUser = threadPackResponseLatestThreadPostUser,
      threadPackResponseWithOrganization = threadPackResponseWithOrganization,
      threadPackResponseWithForum = threadPackResponseWithForum,
      threadPackResponseWithBoard = threadPackResponseWithBoard,
      threadPackResponsePermissions = threadPackResponsePermissions
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ThreadPackResponse where
  toJSON ThreadPackResponse{..} = object $
    [ "tag" .= ("ThreadPackResponse" :: Text)
    , "thread" .= threadPackResponseThread
    , "thread_id" .= threadPackResponseThreadId
    , "user" .= threadPackResponseUser
    , "user_id" .= threadPackResponseUserId
    , "stat" .= threadPackResponseStat
    , "like" .= threadPackResponseLike
    , "star" .= threadPackResponseStar
    , "latest_thread_post" .= threadPackResponseLatestThreadPost
    , "latest_thread_post_user" .= threadPackResponseLatestThreadPostUser
    , "with_organization" .= threadPackResponseWithOrganization
    , "with_forum" .= threadPackResponseWithForum
    , "with_board" .= threadPackResponseWithBoard
    , "permissions" .= threadPackResponsePermissions
    ]


instance Eq ThreadPackResponse where
  (==) a b = threadPackResponseThread a == threadPackResponseThread b && threadPackResponseThreadId a == threadPackResponseThreadId b && threadPackResponseUser a == threadPackResponseUser b && threadPackResponseUserId a == threadPackResponseUserId b && threadPackResponseStat a == threadPackResponseStat b && threadPackResponseLike a == threadPackResponseLike b && threadPackResponseStar a == threadPackResponseStar b && threadPackResponseLatestThreadPost a == threadPackResponseLatestThreadPost b && threadPackResponseLatestThreadPostUser a == threadPackResponseLatestThreadPostUser b && threadPackResponseWithOrganization a == threadPackResponseWithOrganization b && threadPackResponseWithForum a == threadPackResponseWithForum b && threadPackResponseWithBoard a == threadPackResponseWithBoard b && threadPackResponsePermissions a == threadPackResponsePermissions b

instance Show ThreadPackResponse where
    show rec = "threadPackResponseThread: " <> show (threadPackResponseThread rec) <> ", " <> "threadPackResponseThreadId: " <> show (threadPackResponseThreadId rec) <> ", " <> "threadPackResponseUser: " <> show (threadPackResponseUser rec) <> ", " <> "threadPackResponseUserId: " <> show (threadPackResponseUserId rec) <> ", " <> "threadPackResponseStat: " <> show (threadPackResponseStat rec) <> ", " <> "threadPackResponseLike: " <> show (threadPackResponseLike rec) <> ", " <> "threadPackResponseStar: " <> show (threadPackResponseStar rec) <> ", " <> "threadPackResponseLatestThreadPost: " <> show (threadPackResponseLatestThreadPost rec) <> ", " <> "threadPackResponseLatestThreadPostUser: " <> show (threadPackResponseLatestThreadPostUser rec) <> ", " <> "threadPackResponseWithOrganization: " <> show (threadPackResponseWithOrganization rec) <> ", " <> "threadPackResponseWithForum: " <> show (threadPackResponseWithForum rec) <> ", " <> "threadPackResponseWithBoard: " <> show (threadPackResponseWithBoard rec) <> ", " <> "threadPackResponsePermissions: " <> show (threadPackResponsePermissions rec)

data ThreadPackResponses = ThreadPackResponses {
  threadPackResponses :: !([ThreadPackResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadPackResponses where
  parseJSON (Object o) = do
    threadPackResponses <- o .: ("thread_pack_responses" :: Text)
    pure $ ThreadPackResponses {
      threadPackResponses = threadPackResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ThreadPackResponses where
  toJSON ThreadPackResponses{..} = object $
    [ "tag" .= ("ThreadPackResponses" :: Text)
    , "thread_pack_responses" .= threadPackResponses
    ]


instance Eq ThreadPackResponses where
  (==) a b = threadPackResponses a == threadPackResponses b

instance Show ThreadPackResponses where
    show rec = "threadPackResponses: " <> show (threadPackResponses rec)
-- footer