{-# LANGUAGE TemplateHaskell #-}

module Oke.Bootstrap.Context where

import Oke.Bootstrap.CLI
import Oke.Bootstrap.Config
import Oke.Bootstrap.System
import Path

data Context = Context
  { system :: !System,
    cli :: !CLI
  }
  deriving stock (Show, Eq)

mkContext :: IO Context
mkContext = do
  system <- getSystem
  cli <- getCLI
  _ <- getConfig (system.xdgDirs.xdgConfig)
  -- TODO config should be normalized
  pure $ Context system cli

logPath :: Context -> Path Abs File
logPath ctx = ctx.system.xdgDirs.xdgState </> $(mkRelFile "oke.log")

configPath :: Context -> Path Abs File
configPath ctx = ctx.system.xdgDirs.xdgConfig </> $(mkRelFile "config.yaml")

registryPath :: Context -> Path Abs File
registryPath ctx = ctx.system.xdgDirs.xdgState </> $(mkRelFile "cask.json")