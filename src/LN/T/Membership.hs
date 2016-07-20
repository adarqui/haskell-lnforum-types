{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Membership where





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

data Membership
  = Membership_InviteOnly 
  | Membership_RequestInvite 
  | Membership_Join 
  | Membership_Locked 
  deriving (Generic,Typeable,NFData)


instance FromJSON Membership where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("Membership_InviteOnly" :: Text) -> do
        pure Membership_InviteOnly

      ("Membership_RequestInvite" :: Text) -> do
        pure Membership_RequestInvite

      ("Membership_Join" :: Text) -> do
        pure Membership_Join

      ("Membership_Locked" :: Text) -> do
        pure Membership_Locked

      _ -> fail "Could not parse Membership"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Membership where
  toJSON (Membership_InviteOnly ) = object $
    [ "tag" .= ("Membership_InviteOnly" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Membership_RequestInvite ) = object $
    [ "tag" .= ("Membership_RequestInvite" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Membership_Join ) = object $
    [ "tag" .= ("Membership_Join" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Membership_Locked ) = object $
    [ "tag" .= ("Membership_Locked" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq Membership where
  (==) Membership_InviteOnly Membership_InviteOnly = True
  (==) Membership_RequestInvite Membership_RequestInvite = True
  (==) Membership_Join Membership_Join = True
  (==) Membership_Locked Membership_Locked = True
  (==) _ _ = False

instance Show Membership where
  show Membership_InviteOnly = "invite_only"
  show Membership_RequestInvite = "request_invite"
  show Membership_Join = "join"
  show Membership_Locked = "locked"


instance Read Membership where
  readsPrec _ "invite_only" = [(Membership_InviteOnly, "")]
  readsPrec _ "request_invite" = [(Membership_RequestInvite, "")]
  readsPrec _ "join" = [(Membership_Join, "")]
  readsPrec _ "locked" = [(Membership_Locked, "")]
  readsPrec _ _ = []

-- footer