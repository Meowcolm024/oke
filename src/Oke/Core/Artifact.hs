{-# LANGUAGE DeriveAnyClass #-}

module Oke.Core.Artifact where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Path

type Token = Text

data Artifact
  = AppBundle {srcPath :: Path Abs Dir, destPath :: Path Abs Dir}
  | BinaryLink {linkPath :: Path Abs File, targetPath :: Path Abs File}
  | ShellCompletion {shell :: Shell, filePath :: Path Abs File}
  | ManPage {section :: Word, filePath :: Path Abs File}
  | Pkg {receiptPath :: Path Abs File}
  | Preflight -- e.g. system extension loaded, permissions granted
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Shell = Bash | Zsh | Fish
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data InstalledCask = InstalledCask
  { version :: !Text,
    installedAt :: !UTCTime,
    artifacts :: ![Artifact],
    caskToken :: !Token,
    tapSource :: !(Maybe Text) -- e.g. "homebrew/cask", Nothing = local
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
