module Oke.Core.Registry where

import Effectful
import Effectful.FileSystem (FileSystem)
import Oke.Effect.Context
import Oke.Effect.Log
import Oke.Effect.Network
import Path (fromAbsFile)

update :: forall es. (Log :> es, FileSystem :> es, Network :> es, IOE :> es, Ctx :> es) => Eff es ()
update = do
  logDbg "update cask.json"
  ctx <- getContext
  logInfo "Fetching cask.json from https://formulae.brew.sh/api/cask.json"
  response <- request "https://formulae.brew.sh/api/cask.json"
  writeFileBS (fromAbsFile (registryPath ctx)) response
  logInfo "Updated cask.json"

-- TODO keep track of update time
-- TODO error handling
