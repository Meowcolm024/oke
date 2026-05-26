module Oke.Effect.Error where

import Effectful
import Effectful.Error.Static
import Oke.Core.Error
import Oke.Effect.Log

handleRegErr :: forall es. (IOE :> es, Log :> es) => Eff (Error RegError : es) () -> Eff es ()
handleRegErr eff = do
  res <- runErrorNoCallStack eff
  case res of
    Right r -> pure r
    Left RegistryNotFound -> logErr "No cask.json found, run: `oke update`"
    Left RegistryHashMismatch -> logErr "Registry out of sync, run: `oke update`"
    Left RegistryParseFailure -> logErr "cask.json is corrupted, run: `oke update`"
    Left (StoreDecodeError msg) -> logErr $ "Error: " <> msg
