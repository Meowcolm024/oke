{-# LANGUAGE DeriveAnyClass #-}

module Oke.Core.Cask where

import Data.Aeson

data CaskInfo = CaskInfo
  { token :: !Text,
    fullToken :: !Text,
    tap :: !Text,
    name :: ![Text],
    desc :: !Text,
    homepage :: !Text,
    version :: !Text,
    url :: !Text,
    sha256 :: !Text
    -- TODO artifacts
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON CaskInfo where
  parseJSON = withObject "CaskInfo" $ \o ->
    CaskInfo
      <$> o .: "token"
      <*> o .: "full_token"
      <*> o .: "tap"
      <*> o .: "name"
      <*> o .:? "desc" .!= "" -- nullable in API
      <*> o .: "homepage"
      <*> o .: "version"
      <*> o .: "url"
      <*> o .: "sha256"
