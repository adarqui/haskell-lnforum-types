{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.Board where


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
import           Prelude

data BoardPackResponse = BoardPackResponse {
  boardPackResponseBoard :: !(BoardResponse),
  boardPackResponseBoardId :: !(Int64),
  boardPackResponseStat :: !(BoardStatResponse),
  boardPackResponseLike :: !((Maybe LikeResponse)),
  boardPackResponseStar :: !((Maybe StarResponse)),
  boardPackResponseLatestThread :: !((Maybe ThreadResponse)),
  boardPackResponseLatestThreadPost :: !((Maybe ThreadPostResponse)),
  boardPackResponseLatestThreadPostUser :: !((Maybe UserSanitizedResponse)),
  boardPackResponseWithOrganization :: !((Maybe OrganizationResponse)),
  boardPackResponseWithForum :: !((Maybe ForumResponse)),
  boardPackResponsePermissions :: !(Permissions),
  boardPackResponseThreadPermissions :: !(Permissions)
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardPackResponse where
  parseJSON (Object o) = do
    boardPackResponseBoard <- o .: ("board" :: Text)
    boardPackResponseBoardId <- o .: ("board_id" :: Text)
    boardPackResponseStat <- o .: ("stat" :: Text)
    boardPackResponseLike <- o .: ("like" :: Text)
    boardPackResponseStar <- o .: ("star" :: Text)
    boardPackResponseLatestThread <- o .: ("latest_thread" :: Text)
    boardPackResponseLatestThreadPost <- o .: ("latest_thread_post" :: Text)
    boardPackResponseLatestThreadPostUser <- o .: ("latest_thread_post_user" :: Text)
    boardPackResponseWithOrganization <- o .: ("with_organization" :: Text)
    boardPackResponseWithForum <- o .: ("with_forum" :: Text)
    boardPackResponsePermissions <- o .: ("permissions" :: Text)
    boardPackResponseThreadPermissions <- o .: ("thread_permissions" :: Text)
    pure $ BoardPackResponse {
      boardPackResponseBoard = boardPackResponseBoard,
      boardPackResponseBoardId = boardPackResponseBoardId,
      boardPackResponseStat = boardPackResponseStat,
      boardPackResponseLike = boardPackResponseLike,
      boardPackResponseStar = boardPackResponseStar,
      boardPackResponseLatestThread = boardPackResponseLatestThread,
      boardPackResponseLatestThreadPost = boardPackResponseLatestThreadPost,
      boardPackResponseLatestThreadPostUser = boardPackResponseLatestThreadPostUser,
      boardPackResponseWithOrganization = boardPackResponseWithOrganization,
      boardPackResponseWithForum = boardPackResponseWithForum,
      boardPackResponsePermissions = boardPackResponsePermissions,
      boardPackResponseThreadPermissions = boardPackResponseThreadPermissions
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BoardPackResponse where
  toJSON BoardPackResponse{..} = object $
    [ "tag" .= ("BoardPackResponse" :: Text)
    , "board" .= boardPackResponseBoard
    , "board_id" .= boardPackResponseBoardId
    , "stat" .= boardPackResponseStat
    , "like" .= boardPackResponseLike
    , "star" .= boardPackResponseStar
    , "latest_thread" .= boardPackResponseLatestThread
    , "latest_thread_post" .= boardPackResponseLatestThreadPost
    , "latest_thread_post_user" .= boardPackResponseLatestThreadPostUser
    , "with_organization" .= boardPackResponseWithOrganization
    , "with_forum" .= boardPackResponseWithForum
    , "permissions" .= boardPackResponsePermissions
    , "thread_permissions" .= boardPackResponseThreadPermissions
    ]


instance Eq BoardPackResponse where
  (==) a b = boardPackResponseBoard a == boardPackResponseBoard b && boardPackResponseBoardId a == boardPackResponseBoardId b && boardPackResponseStat a == boardPackResponseStat b && boardPackResponseLike a == boardPackResponseLike b && boardPackResponseStar a == boardPackResponseStar b && boardPackResponseLatestThread a == boardPackResponseLatestThread b && boardPackResponseLatestThreadPost a == boardPackResponseLatestThreadPost b && boardPackResponseLatestThreadPostUser a == boardPackResponseLatestThreadPostUser b && boardPackResponseWithOrganization a == boardPackResponseWithOrganization b && boardPackResponseWithForum a == boardPackResponseWithForum b && boardPackResponsePermissions a == boardPackResponsePermissions b && boardPackResponseThreadPermissions a == boardPackResponseThreadPermissions b

instance Show BoardPackResponse where
    show rec = "boardPackResponseBoard: " <> show (boardPackResponseBoard rec) <> ", " <> "boardPackResponseBoardId: " <> show (boardPackResponseBoardId rec) <> ", " <> "boardPackResponseStat: " <> show (boardPackResponseStat rec) <> ", " <> "boardPackResponseLike: " <> show (boardPackResponseLike rec) <> ", " <> "boardPackResponseStar: " <> show (boardPackResponseStar rec) <> ", " <> "boardPackResponseLatestThread: " <> show (boardPackResponseLatestThread rec) <> ", " <> "boardPackResponseLatestThreadPost: " <> show (boardPackResponseLatestThreadPost rec) <> ", " <> "boardPackResponseLatestThreadPostUser: " <> show (boardPackResponseLatestThreadPostUser rec) <> ", " <> "boardPackResponseWithOrganization: " <> show (boardPackResponseWithOrganization rec) <> ", " <> "boardPackResponseWithForum: " <> show (boardPackResponseWithForum rec) <> ", " <> "boardPackResponsePermissions: " <> show (boardPackResponsePermissions rec) <> ", " <> "boardPackResponseThreadPermissions: " <> show (boardPackResponseThreadPermissions rec)

data BoardPackResponses = BoardPackResponses {
  boardPackResponses :: !([BoardPackResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardPackResponses where
  parseJSON (Object o) = do
    boardPackResponses <- o .: ("board_pack_responses" :: Text)
    pure $ BoardPackResponses {
      boardPackResponses = boardPackResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BoardPackResponses where
  toJSON BoardPackResponses{..} = object $
    [ "tag" .= ("BoardPackResponses" :: Text)
    , "board_pack_responses" .= boardPackResponses
    ]


instance Eq BoardPackResponses where
  (==) a b = boardPackResponses a == boardPackResponses b

instance Show BoardPackResponses where
    show rec = "boardPackResponses: " <> show (boardPackResponses rec)
-- footer