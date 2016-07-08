{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Permission where




import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)

data Permission
  = Perm_Create 
  | Perm_Read 
  | Perm_Update 
  | Perm_Delete 
  | Perm_Execute 



instance FromJSON Permission where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("Perm_Create" :: Text) -> do
        pure Perm_Create

      ("Perm_Read" :: Text) -> do
        pure Perm_Read

      ("Perm_Update" :: Text) -> do
        pure Perm_Update

      ("Perm_Delete" :: Text) -> do
        pure Perm_Delete

      ("Perm_Execute" :: Text) -> do
        pure Perm_Execute

      _ -> fail "Could not parse Permission"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Permission where
  toJSON (Perm_Create ) = object $
    [ "tag" .= ("Perm_Create" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Perm_Read ) = object $
    [ "tag" .= ("Perm_Read" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Perm_Update ) = object $
    [ "tag" .= ("Perm_Update" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Perm_Delete ) = object $
    [ "tag" .= ("Perm_Delete" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (Perm_Execute ) = object $
    [ "tag" .= ("Perm_Execute" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq Permission where
  (==) Perm_Create Perm_Create = True
  (==) Perm_Read Perm_Read = True
  (==) Perm_Update Perm_Update = True
  (==) Perm_Delete Perm_Delete = True
  (==) Perm_Execute Perm_Execute = True
  (==) _ _ = False

instance Show Permission where
  show Perm_Create = "perm_create"
  show Perm_Read = "perm_read"
  show Perm_Update = "perm_update"
  show Perm_Delete = "perm_delete"
  show Perm_Execute = "perm_execute"


type Permissions  = [Permission]

-- footer