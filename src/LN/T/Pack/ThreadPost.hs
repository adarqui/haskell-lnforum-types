{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.ThreadPost where


import LN.T.Permission
import LN.T.Organization
import LN.T.User
import LN.T.Forum
import LN.T.Board
import LN.T.Thread
import LN.T.ThreadPost
import LN.T.Like
import LN.T.Star


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

data ThreadPostPackResponse = ThreadPostPackResponse {
  threadPostPackResponseThreadPost :: !(ThreadPostResponse),
  threadPostPackResponseThreadPostId :: !(Int64),
  threadPostPackResponseUser :: !(UserSanitizedResponse),
  threadPostPackResponseUserId :: !(Int64),
  threadPostPackResponseStat :: !(ThreadPostStatResponse),
  threadPostPackResponseLike :: !((Maybe LikeResponse)),
  threadPostPackResponseStar :: !((Maybe StarResponse)),
  threadPostPackResponseWithOrganization :: !((Maybe OrganizationResponse)),
  threadPostPackResponseWithForum :: !((Maybe ForumResponse)),
  threadPostPackResponseWithBoard :: !((Maybe BoardResponse)),
  threadPostPackResponseWithThread :: !((Maybe ThreadResponse)),
  threadPostPackResponsePermissions :: !(Permissions)
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadPostPackResponse where
  parseJSON (Object o) = do
    threadPostPackResponseThreadPost <- o .: ("thread_post" :: Text)
    threadPostPackResponseThreadPostId <- o .: ("thread_post_id" :: Text)
    threadPostPackResponseUser <- o .: ("user" :: Text)
    threadPostPackResponseUserId <- o .: ("user_id" :: Text)
    threadPostPackResponseStat <- o .: ("stat" :: Text)
    threadPostPackResponseLike <- o .: ("like" :: Text)
    threadPostPackResponseStar <- o .: ("star" :: Text)
    threadPostPackResponseWithOrganization <- o .: ("with_organization" :: Text)
    threadPostPackResponseWithForum <- o .: ("with_forum" :: Text)
    threadPostPackResponseWithBoard <- o .: ("with_board" :: Text)
    threadPostPackResponseWithThread <- o .: ("with_thread" :: Text)
    threadPostPackResponsePermissions <- o .: ("permissions" :: Text)
    pure $ ThreadPostPackResponse {
      threadPostPackResponseThreadPost = threadPostPackResponseThreadPost,
      threadPostPackResponseThreadPostId = threadPostPackResponseThreadPostId,
      threadPostPackResponseUser = threadPostPackResponseUser,
      threadPostPackResponseUserId = threadPostPackResponseUserId,
      threadPostPackResponseStat = threadPostPackResponseStat,
      threadPostPackResponseLike = threadPostPackResponseLike,
      threadPostPackResponseStar = threadPostPackResponseStar,
      threadPostPackResponseWithOrganization = threadPostPackResponseWithOrganization,
      threadPostPackResponseWithForum = threadPostPackResponseWithForum,
      threadPostPackResponseWithBoard = threadPostPackResponseWithBoard,
      threadPostPackResponseWithThread = threadPostPackResponseWithThread,
      threadPostPackResponsePermissions = threadPostPackResponsePermissions
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ThreadPostPackResponse where
  toJSON ThreadPostPackResponse{..} = object $
    [ "tag" .= ("ThreadPostPackResponse" :: Text)
    , "thread_post" .= threadPostPackResponseThreadPost
    , "thread_post_id" .= threadPostPackResponseThreadPostId
    , "user" .= threadPostPackResponseUser
    , "user_id" .= threadPostPackResponseUserId
    , "stat" .= threadPostPackResponseStat
    , "like" .= threadPostPackResponseLike
    , "star" .= threadPostPackResponseStar
    , "with_organization" .= threadPostPackResponseWithOrganization
    , "with_forum" .= threadPostPackResponseWithForum
    , "with_board" .= threadPostPackResponseWithBoard
    , "with_thread" .= threadPostPackResponseWithThread
    , "permissions" .= threadPostPackResponsePermissions
    ]


instance Eq ThreadPostPackResponse where
  (==) a b = threadPostPackResponseThreadPost a == threadPostPackResponseThreadPost b && threadPostPackResponseThreadPostId a == threadPostPackResponseThreadPostId b && threadPostPackResponseUser a == threadPostPackResponseUser b && threadPostPackResponseUserId a == threadPostPackResponseUserId b && threadPostPackResponseStat a == threadPostPackResponseStat b && threadPostPackResponseLike a == threadPostPackResponseLike b && threadPostPackResponseStar a == threadPostPackResponseStar b && threadPostPackResponseWithOrganization a == threadPostPackResponseWithOrganization b && threadPostPackResponseWithForum a == threadPostPackResponseWithForum b && threadPostPackResponseWithBoard a == threadPostPackResponseWithBoard b && threadPostPackResponseWithThread a == threadPostPackResponseWithThread b && threadPostPackResponsePermissions a == threadPostPackResponsePermissions b

instance Show ThreadPostPackResponse where
    show rec = "threadPostPackResponseThreadPost: " <> show (threadPostPackResponseThreadPost rec) <> ", " <> "threadPostPackResponseThreadPostId: " <> show (threadPostPackResponseThreadPostId rec) <> ", " <> "threadPostPackResponseUser: " <> show (threadPostPackResponseUser rec) <> ", " <> "threadPostPackResponseUserId: " <> show (threadPostPackResponseUserId rec) <> ", " <> "threadPostPackResponseStat: " <> show (threadPostPackResponseStat rec) <> ", " <> "threadPostPackResponseLike: " <> show (threadPostPackResponseLike rec) <> ", " <> "threadPostPackResponseStar: " <> show (threadPostPackResponseStar rec) <> ", " <> "threadPostPackResponseWithOrganization: " <> show (threadPostPackResponseWithOrganization rec) <> ", " <> "threadPostPackResponseWithForum: " <> show (threadPostPackResponseWithForum rec) <> ", " <> "threadPostPackResponseWithBoard: " <> show (threadPostPackResponseWithBoard rec) <> ", " <> "threadPostPackResponseWithThread: " <> show (threadPostPackResponseWithThread rec) <> ", " <> "threadPostPackResponsePermissions: " <> show (threadPostPackResponsePermissions rec)

data ThreadPostPackResponses = ThreadPostPackResponses {
  threadPostPackResponses :: !([ThreadPostPackResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadPostPackResponses where
  parseJSON (Object o) = do
    threadPostPackResponses <- o .: ("thread_post_pack_responses" :: Text)
    pure $ ThreadPostPackResponses {
      threadPostPackResponses = threadPostPackResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ThreadPostPackResponses where
  toJSON ThreadPostPackResponses{..} = object $
    [ "tag" .= ("ThreadPostPackResponses" :: Text)
    , "thread_post_pack_responses" .= threadPostPackResponses
    ]


instance Eq ThreadPostPackResponses where
  (==) a b = threadPostPackResponses a == threadPostPackResponses b

instance Show ThreadPostPackResponses where
    show rec = "threadPostPackResponses: " <> show (threadPostPackResponses rec)
-- footer