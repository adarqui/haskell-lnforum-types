{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Ent where





import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Int            (Int64)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)
import           Data.Default

data Ent
  = Ent_Organization 
  | Ent_Team 
  | Ent_TeamMember 
  | Ent_GlobalGroup 
  | Ent_Group 
  | Ent_GroupMember 
  | Ent_User 
  | Ent_UserSanitized 
  | Ent_Forum 
  | Ent_Board 
  | Ent_Thread 
  | Ent_ThreadPost 
  | Ent_Blog 
  | Ent_BlogPost 
  | Ent_BlogComment 
  | Ent_Resource 
  | Ent_Leuron 
  | Ent_Comment 
  | Ent_Api 
  | Ent_Like 
  | Ent_Star 
  | Ent_None 



instance FromJSON Ent where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("Ent_Organization" :: Text) -> do
        pure Ent_Organization

      ("Ent_Team" :: Text) -> do
        pure Ent_Team

      ("Ent_TeamMember" :: Text) -> do
        pure Ent_TeamMember

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

      ("Ent_Blog" :: Text) -> do
        pure Ent_Blog

      ("Ent_BlogPost" :: Text) -> do
        pure Ent_BlogPost

      ("Ent_BlogComment" :: Text) -> do
        pure Ent_BlogComment

      ("Ent_Resource" :: Text) -> do
        pure Ent_Resource

      ("Ent_Leuron" :: Text) -> do
        pure Ent_Leuron

      ("Ent_Comment" :: Text) -> do
        pure Ent_Comment

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
  toJSON (Ent_Team ) = object $
    [ "tag" .= ("Ent_Team" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_TeamMember ) = object $
    [ "tag" .= ("Ent_TeamMember" :: Text)
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
  toJSON (Ent_Blog ) = object $
    [ "tag" .= ("Ent_Blog" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_BlogPost ) = object $
    [ "tag" .= ("Ent_BlogPost" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_BlogComment ) = object $
    [ "tag" .= ("Ent_BlogComment" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_Resource ) = object $
    [ "tag" .= ("Ent_Resource" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_Leuron ) = object $
    [ "tag" .= ("Ent_Leuron" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Ent_Comment ) = object $
    [ "tag" .= ("Ent_Comment" :: Text)
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
  (==) Ent_Team Ent_Team = True
  (==) Ent_TeamMember Ent_TeamMember = True
  (==) Ent_GlobalGroup Ent_GlobalGroup = True
  (==) Ent_Group Ent_Group = True
  (==) Ent_GroupMember Ent_GroupMember = True
  (==) Ent_User Ent_User = True
  (==) Ent_UserSanitized Ent_UserSanitized = True
  (==) Ent_Forum Ent_Forum = True
  (==) Ent_Board Ent_Board = True
  (==) Ent_Thread Ent_Thread = True
  (==) Ent_ThreadPost Ent_ThreadPost = True
  (==) Ent_Blog Ent_Blog = True
  (==) Ent_BlogPost Ent_BlogPost = True
  (==) Ent_BlogComment Ent_BlogComment = True
  (==) Ent_Resource Ent_Resource = True
  (==) Ent_Leuron Ent_Leuron = True
  (==) Ent_Comment Ent_Comment = True
  (==) Ent_Api Ent_Api = True
  (==) Ent_Like Ent_Like = True
  (==) Ent_Star Ent_Star = True
  (==) Ent_None Ent_None = True
  (==) _ _ = False

instance Show Ent where
  show Ent_Organization = "organization"
  show Ent_Team = "team"
  show Ent_TeamMember = "team_member"
  show Ent_GlobalGroup = "global_group"
  show Ent_Group = "group"
  show Ent_GroupMember = "group_member"
  show Ent_User = "user"
  show Ent_UserSanitized = "user_sanitized"
  show Ent_Forum = "forum"
  show Ent_Board = "board"
  show Ent_Thread = "thread"
  show Ent_ThreadPost = "thread_post"
  show Ent_Blog = "blog"
  show Ent_BlogPost = "blog_post"
  show Ent_BlogComment = "blog_comment"
  show Ent_Resource = "resource"
  show Ent_Leuron = "leuron"
  show Ent_Comment = "comment"
  show Ent_Api = "api"
  show Ent_Like = "like"
  show Ent_Star = "star"
  show Ent_None = "none"


instance Read Ent where
  readsPrec _ "organization" = [(Ent_Organization, "")]
  readsPrec _ "team" = [(Ent_Team, "")]
  readsPrec _ "team_member" = [(Ent_TeamMember, "")]
  readsPrec _ "global_group" = [(Ent_GlobalGroup, "")]
  readsPrec _ "group" = [(Ent_Group, "")]
  readsPrec _ "group_member" = [(Ent_GroupMember, "")]
  readsPrec _ "user" = [(Ent_User, "")]
  readsPrec _ "user_sanitized" = [(Ent_UserSanitized, "")]
  readsPrec _ "forum" = [(Ent_Forum, "")]
  readsPrec _ "board" = [(Ent_Board, "")]
  readsPrec _ "thread" = [(Ent_Thread, "")]
  readsPrec _ "thread_post" = [(Ent_ThreadPost, "")]
  readsPrec _ "blog" = [(Ent_Blog, "")]
  readsPrec _ "blog_post" = [(Ent_BlogPost, "")]
  readsPrec _ "blog_comment" = [(Ent_BlogComment, "")]
  readsPrec _ "resource" = [(Ent_Resource, "")]
  readsPrec _ "leuron" = [(Ent_Leuron, "")]
  readsPrec _ "comment" = [(Ent_Comment, "")]
  readsPrec _ "api" = [(Ent_Api, "")]
  readsPrec _ "like" = [(Ent_Like, "")]
  readsPrec _ "star" = [(Ent_Star, "")]
  readsPrec _ "none" = [(Ent_None, "")]
  readsPrec _ _ = []

-- footer