{-# LANGUAGE TemplateHaskell #-}

module Oke.App.Config where

import Path

type ConfigPath = Path Abs File

-- TODO maybe accept both .yaml and .yml
mkConfigPath :: Path Abs Dir -> ConfigPath
mkConfigPath dir = dir </> $(mkRelFile "config.yaml")

data Config = Config
  {
  }
  deriving stock (Show, Eq)

getConfig :: Path Abs Dir -> IO Config
getConfig _ = pure $ Config {}