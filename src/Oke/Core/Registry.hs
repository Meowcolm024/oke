{-# LANGUAGE TemplateHaskell #-}

module Oke.Core.Registry where

import Data.Time qualified as T
import Effectful
import Network.URI.Static
import Oke.App.Env
import Oke.Effect
import Oke.Effect.FileSystem
import Oke.Effect.Network
import Oke.Effect.Time

update :: forall es. (Time :> es, Log :> es, Network :> es, Ctx :> es) => Eff es ()
update = do
  logDbg "update cask.json"
  ctx <- getCtx
  let caskURI = $$(staticURI "https://formulae.brew.sh/api/cask.json")
  downloadWithStatus caskURI (registryPath ctx)
  time <- getCurrentTime
  logInfo $ "Updated cask.json " <> show time

checkUpdate :: forall es. (Time :> es, FileSystem :> es, Log :> es, Ctx :> es) => Eff es ()
checkUpdate = do
  ctx <- getCtx
  exists <- doesFileExist (registryPath ctx)
  if not exists
    then logInfo "cask.json does not exist, please run `oke update`"
    else do
      time <- getModificationTime (registryPath ctx)
      today <- getCurrentTime
      when (T.diffUTCTime today time > T.nominalDay * 14) $ do
        logInfo "cask.json is outdated (updated over 14 days ago)"
