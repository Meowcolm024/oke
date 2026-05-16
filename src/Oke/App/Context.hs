module Oke.App.Context where

import Oke.App.Bootstrap
import Oke.App.CLI
import Oke.App.Config

-- TODO readonly runtime context

data Context = Context
  { bootstrap :: Bootstrap,
    config :: Config,
    cli :: CLI
    -- should be some other context depending on
  }
  deriving stock (Show, Eq)

getContext :: IO Context
getContext = do
  bootstrap <- getBootstrap
  cli <- getCLI
  config <- getConfig (bootstrap.xdgDirs.xdgConfig)
  pure $ mkContext bootstrap config cli

-- TODO placeholder
mkContext :: Bootstrap -> Config -> CLI -> Context
mkContext = Context