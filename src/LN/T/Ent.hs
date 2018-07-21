{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Ent where





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

data Ent
  = Ent_Organization 
  | Ent_GlobalGroup 
  | Ent_Group 
  | Ent_GroupMember 
  | Ent_User 
  | Ent_UserSanitized 
  | Ent_Forum 
  | Ent_Board 
  | Ent_Thread 
  | Ent_ThreadPost 
  | Ent_Api 
  | Ent_Like 
  | Ent_Star 
  | Ent_None 
  deriving (Generic,Typeable,NFData)


instance FromJSON Ent where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("Ent_Organization" :: Text) -> do
        pure Ent_Organization

      ("Ent_GlobalGroup" :: Text) -> do
        pure Ent_GlobalGroup

      ("Ent_Group" :: Text) -> do
        pure Ent_Group

      ("Ent_GroupMember" :: Text) -> do
        pure Ent_GroupMember

      ("Ent_User" :: Text) -> do
        pure Ent_User

      ("Ent_UserSanitized" :: Text) -> do
        pure Ent_UserSanitized

      ("Ent_Forum" :: Text) -> do
        pure Ent_Forum

      ("Ent_Board" :: Text) -> do
        pure Ent_Board

      ("Ent_Thread" :: Text) -> do
        pure Ent_Thread

      ("Ent_ThreadPost" :: Text) -> do
        pure Ent_ThreadPost

      ("Ent_Api" :: Text) -> do
        pure Ent_Api

      ("Ent_Like" :: Text) -> do
        pure Ent_Like

      ("Ent_Star" :: Text) -> do
        pure Ent_Star

      ("Ent_None" :: Text) -> do
        pure Ent_None

      _ -> fail "Could not parse Ent"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Ent where
  toJSON (Ent_Organization ) = object $
    [ "tag" .= ("Ent_Organization" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_GlobalGroup ) = object $
    [ "tag" .= ("Ent_GlobalGroup" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_Group ) = object $
    [ "tag" .= ("Ent_Group" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_GroupMember ) = object $
    [ "tag" .= ("Ent_GroupMember" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_User ) = object $
    [ "tag" .= ("Ent_User" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_UserSanitized ) = object $
    [ "tag" .= ("Ent_UserSanitized" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_Forum ) = object $
    [ "tag" .= ("Ent_Forum" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_Board ) = object $
    [ "tag" .= ("Ent_Board" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_Thread ) = object $
    [ "tag" .= ("Ent_Thread" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_ThreadPost ) = object $
    [ "tag" .= ("Ent_ThreadPost" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_Api ) = object $
    [ "tag" .= ("Ent_Api" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_Like ) = object $
    [ "tag" .= ("Ent_Like" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_Star ) = object $
    [ "tag" .= ("Ent_Star" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_None ) = object $
    [ "tag" .= ("Ent_None" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq Ent where
  (==) Ent_Organization Ent_Organization = True
  (==) Ent_GlobalGroup Ent_GlobalGroup = True
  (==) Ent_Group Ent_Group = True
  (==) Ent_GroupMember Ent_GroupMember = True
  (==) Ent_User Ent_User = True
  (==) Ent_UserSanitized Ent_UserSanitized = True
  (==) Ent_Forum Ent_Forum = True
  (==) Ent_Board Ent_Board = True
  (==) Ent_Thread Ent_Thread = True
  (==) Ent_ThreadPost Ent_ThreadPost = True
  (==) Ent_Api Ent_Api = True
  (==) Ent_Like Ent_Like = True
  (==) Ent_Star Ent_Star = True
  (==) Ent_None Ent_None = True
  (==) _ _ = False

instance Show Ent where
  show Ent_Organization = "organization"
  show Ent_GlobalGroup = "global_group"
  show Ent_Group = "group"
  show Ent_GroupMember = "group_member"
  show Ent_User = "user"
  show Ent_UserSanitized = "user_sanitized"
  show Ent_Forum = "forum"
  show Ent_Board = "board"
  show Ent_Thread = "thread"
  show Ent_ThreadPost = "thread_post"
  show Ent_Api = "api"
  show Ent_Like = "like"
  show Ent_Star = "star"
  show Ent_None = "none"


instance Read Ent where
  readsPrec _ "organization" = [(Ent_Organization, "")]
  readsPrec _ "global_group" = [(Ent_GlobalGroup, "")]
  readsPrec _ "group" = [(Ent_Group, "")]
  readsPrec _ "group_member" = [(Ent_GroupMember, "")]
  readsPrec _ "user" = [(Ent_User, "")]
  readsPrec _ "user_sanitized" = [(Ent_UserSanitized, "")]
  readsPrec _ "forum" = [(Ent_Forum, "")]
  readsPrec _ "board" = [(Ent_Board, "")]
  readsPrec _ "thread" = [(Ent_Thread, "")]
  readsPrec _ "thread_post" = [(Ent_ThreadPost, "")]
  readsPrec _ "api" = [(Ent_Api, "")]
  readsPrec _ "like" = [(Ent_Like, "")]
  readsPrec _ "star" = [(Ent_Star, "")]
  readsPrec _ "none" = [(Ent_None, "")]
  readsPrec _ _ = []

-- footer