{-# LANGUAGE TemplateHaskell #-}

module Oke.Core.Registry where

import Effectful
import Network.URI.Static
import Oke.Effect.Context
import Oke.Effect.Log
import Oke.Effect.Network

update :: forall es. (Log :> es, Network :> es, Ctx :> es) => Eff es ()
update = do
  logDbg "update cask.json"
  ctx <- getContext
  let caskURI = $$(staticURI "https://formulae.brew.sh/api/cask.json")
  logInfo $ "Fetching cask.json from " <> show caskURI
  download caskURI (registryPath ctx)
  logInfo "Updated cask.json"

-- TODO keep track of update time
-- TODO error handling
