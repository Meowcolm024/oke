{-# LANGUAGE TemplateHaskell #-}

module Oke.Bootstrap.Context (Context (..), mkContext) where

import Oke.Bootstrap.CLI
import Oke.Bootstrap.Config
import Oke.Bootstrap.Log
import Oke.Bootstrap.System
import Path

data Context = Context
  { system :: System,
    cli :: CLI,
    logPath :: LogPath,
    configPath :: ConfigPath
  }
  deriving stock (Show, Eq)

mkContext :: IO Context
mkContext = do
  system <- getSystem
  let logPath = system.xdgDirs.xdgState </> $(mkRelFile "oke.log")
  cli <- getCLI
  _ <- getConfig (system.xdgDirs.xdgConfig)
  let configPath = system.xdgDirs.xdgConfig </> $(mkRelFile "config.yaml")
  -- TODO config should be normalized
  pure $ Context system cli logPath configPath
