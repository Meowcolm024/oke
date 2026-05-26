{-# LANGUAGE DeriveAnyClass #-}

module Oke.Core.State where

import Data.Aeson
import Data.Time (UTCTime)
import Oke.Core.Artifact

data AppState = AppState
  { registryUpdateTime :: !(Maybe UTCTime),
    registryHash :: !(Maybe Text),
    installedCasks :: !(Map Text InstalledCask)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyAppState :: AppState
emptyAppState = AppState Nothing Nothing mempty
