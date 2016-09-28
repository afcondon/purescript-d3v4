module Data.MediaType where

import Prelude

import Data.Generic (class Generic)

newtype MediaType = MediaType String

unMediaType :: MediaType -> String
unMediaType (MediaType s) = s

derive instance eqMediaType :: Eq MediaType
derive instance ordMediaType :: Ord MediaType
derive instance genericMediaType :: Generic MediaType

instance showMediaType :: Show MediaType where
  show (MediaType h) = "(MediaType " <> show h <> ")"
