{-# LANGUAGE TemplateHaskell #-}

module Oke.Bootstrap.Config where

import Path

type ConfigPath = Path Abs File

data Config = Config
  {
  }
  deriving stock (Show, Eq)

getConfig :: Path Abs Dir -> IO Config
getConfig _ = pure $ Config {}