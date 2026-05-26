{-# LANGUAGE TemplateHaskell #-}

module Oke.App.Config where

import Path

-- TODO what config do we need?
data Config = Config
  {
  }
  deriving stock (Show, Eq)

getConfig :: Path Abs Dir -> IO Config
getConfig _ = pure $ Config {}
