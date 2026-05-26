{-# LANGUAGE TemplateHaskell #-}

module Oke.Core.Registry where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (decodeStrict)
import Data.ByteString.Base16 qualified as B16
import Data.Time qualified as T
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem.IO.ByteString
import Network.URI.Static
import Oke.App.Env (registryPath)
import Oke.Core.Cask (Registry)
import Oke.Core.Error
import Oke.Core.State
import Oke.Effect
import Oke.Effect.FileSystem
import Oke.Effect.Network (downloadWithStatus)
import Oke.Effect.Store
import Oke.Effect.Time (getCurrentTime)

update ::
  forall es.
  ( Log :> es,
    Network :> es,
    Ctx :> es,
    FileSystem :> es,
    Time :> es,
    Store AppState :> es
  ) =>
  Eff es ()
update = do
  logDbg "update cask.json"
  ctx <- getCtx
  let caskURI = $$(staticURI "https://formulae.brew.sh/api/cask.json")
  downloadWithStatus caskURI (registryPath ctx)
  time <- getCurrentTime
  hash <- withBinaryFile (registryPath ctx) ReadMode $ \handle -> SHA256.hash <$> hGetContents handle
  modifyStore $ \st ->
    st
      { registryUpdateTime = Just time,
        registryHash = Just $ decodeUtf8 (B16.encode hash)
      }
  logInfo $ "Updated cask.json " <> show time

getRegistry ::
  forall es.
  ( Log :> es,
    Ctx :> es,
    FileSystem :> es,
    Store AppState :> es,
    Time :> es,
    Error RegError :> es
  ) =>
  Eff es Registry
getRegistry = do
  ctx <- getCtx
  unlessM (doesFileExist (registryPath ctx)) $
    throwError RegistryNotFound
  -- check update
  time <- getModificationTime (registryPath ctx)
  today <- getCurrentTime
  when (T.diffUTCTime today time > T.nominalDay * 14) $ do
    logWarn "cask.json is outdated (updated over 14 days ago)"
  -- load cask.json
  (reg, hash) <- withBinaryFile (registryPath ctx) ReadMode $ \handle -> do
    file <- hGetContents handle
    pure (file, decodeUtf8 (B16.encode (SHA256.hash file)))
  logDbg $ "fs cask.json hash = " <> hash
  savedHash <- registryHash <$> getStore
  logDbg $ "saved cask.json hash = " <> fromMaybe "<none>" savedHash
  -- check hash
  case savedHash of
    Just h | h == hash -> pure ()
    _ -> throwError RegistryHashMismatch
  -- decode registry
  case decodeStrict @Registry reg of
    Nothing -> throwError RegistryParseFailure
    Just registry -> pure registry
