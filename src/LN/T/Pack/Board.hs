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


import LN.T.Board
import LN.T.User
import LN.T.Permission
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
  boardPackResponseUser :: !(UserSanitizedResponse),
  boardPackResponseUserId :: !(Int64),
  boardPackResponseStat :: !(BoardStatResponse),
  boardPackResponseLike :: !((Maybe LikeResponse)),
  boardPackResponseStar :: !((Maybe StarResponse)),
  boardPackResponsePermissions :: !(Permissions)
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardPackResponse where
  parseJSON (Object o) = do
    boardPackResponseBoard <- o .: ("board" :: Text)
    boardPackResponseBoardId <- o .: ("board_id" :: Text)
    boardPackResponseUser <- o .: ("user" :: Text)
    boardPackResponseUserId <- o .: ("user_id" :: Text)
    boardPackResponseStat <- o .: ("stat" :: Text)
    boardPackResponseLike <- o .: ("like" :: Text)
    boardPackResponseStar <- o .: ("star" :: Text)
    boardPackResponsePermissions <- o .: ("permissions" :: Text)
    pure $ BoardPackResponse {
      boardPackResponseBoard = boardPackResponseBoard,
      boardPackResponseBoardId = boardPackResponseBoardId,
      boardPackResponseUser = boardPackResponseUser,
      boardPackResponseUserId = boardPackResponseUserId,
      boardPackResponseStat = boardPackResponseStat,
      boardPackResponseLike = boardPackResponseLike,
      boardPackResponseStar = boardPackResponseStar,
      boardPackResponsePermissions = boardPackResponsePermissions
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BoardPackResponse where
  toJSON BoardPackResponse{..} = object $
    [ "tag" .= ("BoardPackResponse" :: Text)
    , "board" .= boardPackResponseBoard
    , "board_id" .= boardPackResponseBoardId
    , "user" .= boardPackResponseUser
    , "user_id" .= boardPackResponseUserId
    , "stat" .= boardPackResponseStat
    , "like" .= boardPackResponseLike
    , "star" .= boardPackResponseStar
    , "permissions" .= boardPackResponsePermissions
    ]


instance Eq BoardPackResponse where
  (==) a b = boardPackResponseBoard a == boardPackResponseBoard b && boardPackResponseBoardId a == boardPackResponseBoardId b && boardPackResponseUser a == boardPackResponseUser b && boardPackResponseUserId a == boardPackResponseUserId b && boardPackResponseStat a == boardPackResponseStat b && boardPackResponseLike a == boardPackResponseLike b && boardPackResponseStar a == boardPackResponseStar b && boardPackResponsePermissions a == boardPackResponsePermissions b

instance Show BoardPackResponse where
    show rec = "boardPackResponseBoard: " <> show (boardPackResponseBoard rec) <> ", " <> "boardPackResponseBoardId: " <> show (boardPackResponseBoardId rec) <> ", " <> "boardPackResponseUser: " <> show (boardPackResponseUser rec) <> ", " <> "boardPackResponseUserId: " <> show (boardPackResponseUserId rec) <> ", " <> "boardPackResponseStat: " <> show (boardPackResponseStat rec) <> ", " <> "boardPackResponseLike: " <> show (boardPackResponseLike rec) <> ", " <> "boardPackResponseStar: " <> show (boardPackResponseStar rec) <> ", " <> "boardPackResponsePermissions: " <> show (boardPackResponsePermissions rec)

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