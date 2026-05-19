{-# LANGUAGE TemplateHaskell #-}

module Oke.Core.Registry where

import Effectful
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString qualified as FS
import Network.URI.Static
import Oke.Effect.Context
import Oke.Effect.Log
import Oke.Effect.Network
import Path (fromAbsFile)

update :: forall es. (Log :> es, FileSystem :> es, Network :> es, Ctx :> es) => Eff es ()
update = do
  logDbg "update cask.json"
  ctx <- getContext
  let caskURI = $$(staticURI "https://formulae.brew.sh/api/cask.json")
  logInfo $ "Fetching cask.json from " <> show caskURI
  response <- request caskURI
  FS.writeFile (fromAbsFile (registryPath ctx)) response
  logInfo "Updated cask.json"

-- TODO keep track of update time
-- TODO error handling
